module Main
  ( main,
  )
where

import CNix
import qualified Hercules.Agent.Worker (main)
-- import Hercules.Agent.Worker.Build.Prefetched
import qualified Hercules.Agent.Worker.Build.Logger as Logger
import Protolude
import System.IO

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  CNix.init
  -- CNix.setDebug
  Logger.initLogger
  Hercules.Agent.Worker.main
{-
    CNix.withStore $ \store -> do
      let inputDrvOutputs =
            [ "/nix/store/r3vwha4knvqgqdvbvgmz7gd2i0x10x68-default-Setup-setup"
            , "/nix/store/qghrkvk86f9llfkcr1bxsypqbw1a4qmw-stdenv-linux"
            , "/nix/store/rm1hz1lybxangc8sdl7xvzs5dcvigvf7-bash-4.4-p23"
            , "/nix/store/jllaz7cvdmbfa4v3l6646q7pyx536s3j-hercules-server-0.1.0.0-lib-hercules-server-ghc-8.6.5-env"
            , "/nix/store/9sl95r0rrqs8mqs2jd22ydwyi8zs8imb-remove-references-to"
            , "/nix/store/3n3qzr0il9mqg75x0yyxj0c1w2p4448c-hercules-server-0.1.0.0-lib-hercules-server-config"
            , "/nix/store/nl67flma20ixa0x5jms4wk0yfbx4c9wb-glibc-locales-2.27"
            ]
      for_ inputDrvOutputs $ \drv ->
        CNix.ensurePath store drv
      print =<< buildDerivation store "/nix/store/jd2gvddrymzhnz8vha2xdpa6ld5par7n-hercules-server-0.1.0.0-lib-hercules-server.drv" inputDrvOutputs
-}
