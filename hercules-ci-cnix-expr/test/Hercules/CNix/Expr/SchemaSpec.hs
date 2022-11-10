{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Hercules.CNix.Expr.SchemaSpec where

import Hercules.CNix.Expr (EvalState, NixPath, NixString)
import qualified Hercules.CNix.Expr as Expr
import Hercules.CNix.Expr.Raw (RawValueType (Attrs, Bool, Lambda, Null, String))
import Hercules.CNix.Expr.Schema
import Hercules.CNix.Expr.Schema (FromPSObject (fromPSObject))
import Protolude hiding (TypeError, check, evalState)
import SingleState (evalState)
import Test.Hspec

runES :: ReaderT (Ptr EvalState) m a -> m a
runES m = runReaderT m evalState

displaying :: Exception e => [Char] -> e -> Bool
displaying text e = displayException e == text

spec :: Spec
spec = do
  describe "exprWithBasePath" $ do
    it "can get a byte string from an expression" \() -> do
      s <- runES do
        e <- exprWithBasePath "''hi there''" "/" (Proxy @NixString)
        getByteString_ e
      s `shouldBe` "hi there"

  describe "getText_" $ do
    it "can get text from an expression" \() -> do
      s <- runES do
        e <- exprWithBasePath "''hi there''" "/" (Proxy @NixString)
        getText_ e
      s `shouldBe` "hi there"

    it "can report invalid utf8 with provenance" \() -> do
      ( runES do
          e <- exprWithBasePathBS "''hi\xffthere''" "/" (Proxy @NixString)
          getText_ e
        )
        `shouldThrow` \case
          InvalidText p _e -> p == Other "internal expression"
          _ -> False

  describe "check" $ do
    it "reports type errors with provenance" \() -> do
      ( runES do
          e <- exprWithBasePath "true" "/" (Proxy @NixString)
          check e >>= liftIO . Expr.getStringIgnoreContext
        )
        `shouldThrow` (== TypeError (Other "internal expression") Bool [String])

    it "reports type errors with provenance in human readable format" \_ -> do
      displayException (TypeError (Other "internal expression") Bool [String])
        `shouldBe` "Expecting a value of type String, but got type Bool.\n  in internal expression"

  describe "#. and >>." do
    it "report errors with attribute path in provenance" \() -> do
      ( runES do
          e <-
            exprWithBasePath
              "{ a.b.c = true; }"
              "/"
              ( Proxy
                  @( Attrs
                       '[ "a" ::. NixString
                        ]
                   )
              )
          getByteString_ =<< e #. #a
        )
        `shouldThrow` (== TypeError (Attribute (Other "internal expression") "a") Attrs [String])

    it "report errors with attribute path in provenance" \() -> do
      ( runES do
          e <-
            exprWithBasePath
              "{ a.b.c = true; }"
              "/"
              ( Proxy
                  @( Attrs
                       '[ "a"
                            ::. Attrs
                                  '[ "b"
                                       ::. Attrs '["c" ::. NixString]
                                   ]
                        ]
                   )
              )
          getByteString_ =<< e #. #a >>. #b >>. #c
        )
        `shouldThrow` (== TypeError (Attribute (Other "internal expression") "a" `Attribute` "b" `Attribute` "c") Bool [String])

    it "is not strict in the schema" \() -> do
      r <- runES do
        e <-
          exprWithBasePath
            @( Attrs
                 '[ "doesNotExistButNoProblem" ::. NixPath,
                    "a" ::. NixString
                  ]
             )
            "{ a = ''hello there''; }"
            "/"
            Proxy

        getByteString_ =<< e #. #a
      r `shouldBe` "hello there"

  let schema =
        Proxy
          @( Attrs
               '[ "optionallyFunction" ::. (NixString ->? NixString),
                  "optionalAttr" ::? NixString,
                  "nullableAttr" ::?? [StringWithoutContext]
                ]
           )

  describe "$? and >>$?" do
    it "can ignore an optional function" $ \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ optionallyFunction = ''simple as that''; }"
            "/"
            schema
        e #. #optionallyFunction >>$? panic "not needed" >>= getByteString_
      r `shouldBe` "simple as that"

    it "can call an optional function" $ \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ optionallyFunction = what: ''simple as ${what}''; }"
            "/"
            schema
        e #. #optionallyFunction >>$? toPSObject ("a b c" :: Text) >>= getByteString_
      r `shouldBe` "simple as a b c"

    it "can throw an error message with complete info" $ \_ -> do
      -- maybe a new class that returns the allowable types for any schema?
      ( runES do
          e <-
            exprWithBasePath
              "{ optionallyFunction = true; }"
              "/"
              schema
          e #. #optionallyFunction >>$? toPSObject ("a b c" :: Text) >>= getByteString_
        )
        `shouldThrow` (== TypeError (Attribute (Other "internal expression") "optionallyFunction") Bool [Lambda, String])

  describe ".#? and >>?" do
    it "can return Nothing" \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ }"
            "/"
            schema
        e #? #optionalAttr >>= traverse getByteString_
      r `shouldBe` Nothing

    it "can return Just" \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ optionalAttr = ''nice''; }"
            "/"
            schema
        e #? #optionalAttr >>= traverse getByteString_
      r `shouldBe` Just "nice"

  describe ".#??" do
    it "can return Nothing for unset" \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ }"
            "/"
            schema
        e #?? #nullableAttr >>= traverse fromPSObject
      r `shouldBe` (Nothing :: Maybe [ByteString])

    it "can return Nothing for null" \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ nullableAttr = null; }"
            "/"
            schema
        e #?? #nullableAttr >>= traverse fromPSObject
      r `shouldBe` (Nothing :: Maybe [ByteString])

    it "can return Just" \_ -> do
      r <- runES do
        e <-
          exprWithBasePath
            "{ nullableAttr = [''nice'']; }"
            "/"
            schema
        e #?? #nullableAttr >>= traverse fromPSObject
      r `shouldBe` Just ["nice" :: ByteString]

  describe "fromPSObject" do
    describe "@Bool" do
      describe "@Bool" do
        it "can return true" do
          r <- runES do
            e <- exprWithBasePath "true" "/" (Proxy @Bool)
            fromPSObject e
          r `shouldBe` True
        it "can return false" do
          r <- runES do
            e <- exprWithBasePath "false" "/" (Proxy @Bool)
            fromPSObject e
          r `shouldBe` False
        it "can throw a type error" do
          ( runES do
              e <- exprWithBasePath "null" "/" (Proxy @Bool)
              fromPSObject @_ @Bool e
            )
            `shouldThrow` (== TypeError (Other "internal expression") Null [Bool])
        it "can throw a type error with provenance" do
          ( runES do
              e <- exprWithBasePath "{ a = null; }" "/" (Proxy @(Attrs '["a" ::. Bool]))
              fromPSObject @_ @Bool =<< e #. #a
            )
            `shouldThrow` (== TypeError (Other "internal expression" `Attribute` "a") Null [Bool])

    describe "@StringWithoutContext" do
      describe "@ByteString" do
        it "can return the string" do
          r <- runES do
            e <- exprWithBasePath "''hi''" "/" (Proxy @StringWithoutContext)
            fromPSObject e
          r `shouldBe` ("hi" :: ByteString)
      describe "@ByteString" do
        it "can fail because of context" do
          ( runES do
              e <- exprWithBasePath "''hi ${derivation {name = ''pkg''; builder = ''foo''; system = ''x86_64-linux'';}}''" "/" (Proxy @StringWithoutContext)
              fromPSObject @_ @ByteString e
            )
            `shouldThrow` (== StringContextNotAllowed (Other "internal expression"))
      describe "@Text" do
        it "can return the string" do
          r <- runES do
            e <- exprWithBasePath "''hi''" "/" (Proxy @StringWithoutContext)
            fromPSObject e
          r `shouldBe` ("hi" :: Text)
      describe "@Text" do
        it "can fail because of context" do
          ( runES do
              e <- exprWithBasePath "''hi ${derivation {name = ''pkg''; builder = ''foo''; system = ''x86_64-linux'';}}''" "/" (Proxy @StringWithoutContext)
              fromPSObject @_ @Text e
            )
            `shouldThrow` (== StringContextNotAllowed (Other "internal expression"))
      describe "@Text" do
        it "can fail because of utf8" do
          ( runES do
              e <- exprWithBasePathBS "''hi\xffthere''" "/" (Proxy @StringWithoutContext)
              fromPSObject @_ @Text e
            )
            `shouldThrow` \case
              InvalidText p _e -> p == Other "internal expression"
              _ -> False
