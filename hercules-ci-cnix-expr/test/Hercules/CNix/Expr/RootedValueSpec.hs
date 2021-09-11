{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Hercules.CNix.Expr.RootedValueSpec where

import Hercules.CNix.Expr (NixPath, NixString, checkType)
import qualified Hercules.CNix.Expr as Expr
import Hercules.CNix.Expr.RootedValue
import Protolude hiding (TypeError, evalState)
import SingleState (evalState)
import Test.Hspec

run' m = runReaderT (run m) evalState

runES m = runReaderT m evalState

spec :: Spec
spec = do
  describe "" $ do
    it "can marshall a string" $ \() -> do
      (p, s) <- run' "hi"
      p `shouldBe` Data
      s' <- traverse Expr.getStringIgnoreContext =<< checkType evalState s
      s' `shouldBe` Just "hi"
    it "can marshall from an expression" \() -> do
      s <- runES do
        let e = evalWithBasePath "''hi there''" "/" (Proxy @NixString)
        getStringIgnoreContext e
      s `shouldBe` "hi there"
    it "reports type errors with provenance" \() -> do
      ( runES do
          let e = evalWithBasePath "true" "/" (Proxy @NixString)
          getStringIgnoreContext e
        )
        `shouldThrow` (== TypeError (Other "internal expression") "Expected a string, but got Bool")
    it "report errors with attribute path in provenance" \() -> do
      ( runES do
          let e =
                evalWithBasePath
                  "{ a.b.c = true; }"
                  "/"
                  ( Proxy
                      @( Attrs
                           '[ "a" ':. NixString
                            ]
                       )
                  )
          getStringIgnoreContext $ e ^# #a
        )
        `shouldThrow` (== TypeError (Attribute (Other "internal expression") "a") "Expected a string, but got Attrs")
    it "report errors with attribute path in provenance" \() -> do
      ( runES do
          e <-
            memo $
              evalWithBasePath
                "{ a.b.c = true; }"
                "/"
                ( Proxy
                    @( Attrs
                         '[ "a"
                              ':. Attrs
                                    '[ "b"
                                         ':. Attrs '["c" ':. NixString]
                                     ]
                          ]
                     )
                )
          getStringIgnoreContext $ e ^# #a ^# #b ^# #c
        )
        `shouldThrow` (== TypeError (Attribute (Other "internal expression") "a" `Attribute` "b" `Attribute` "c") "Expected a string, but got Bool")
    it "is not strict in the schema" \() -> do
      r <- runES do
        e <-
          memo $
            evalWithBasePath
              @( Attrs
                   '[ "doesNotExistButNoProblem" ':. NixPath,
                      "a" ':. NixString
                    ]
               )
              "{ a = ''hello there''; }"
              "/"
              Proxy

        getStringIgnoreContext $ e ^# #a
      r `shouldBe` "hello there"
