{-# LANGUAGE BlockArguments #-}

module Hercules.CNix.Store.DerivationSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Hercules.CNix.Store (getDerivationArguments, getDerivationBuilder, getDerivationEnv, getDerivationFromString, getDerivationInputs, getDerivationOutputs, getDerivationPlatform, getDerivationSources)
import Hercules.CNix.Store.TestUtil (withTempStore)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  it "parses a fixed-output derivation correctly" $ withTempStore \store -> do
    bs <- BS.readFile "test/data/fixed-output.drv"
    let drvName = "hello-2.10.tar.gz"
    d <- getDerivationFromString store drvName bs
    platform <- getDerivationPlatform d
    platform `shouldBe` "x86_64-linux"
    builder <- getDerivationBuilder d
    builder `shouldBe` "/nix/store/a3fc4zqaiak11jks9zd579mz5v0li8bg-bash-4.4-p23/bin/bash"
    arguments <- getDerivationArguments d
    arguments `shouldBe` ["-e", "/nix/store/720ikgx7yaapyb8hvi8lkicjqwzcx3xr-builder.sh"]
    sources <- getDerivationSources store d
    show' sources `shouldBe` "[720ikgx7yaapyb8hvi8lkicjqwzcx3xr-builder.sh]"
    inputs <- getDerivationInputs store d
    show' inputs `shouldBe` "[(07apmkf04yjy8scnvz6xh08377kfdyhq-mirrors-list.drv,[\"out\"]),(abwabzabxq9vgd1rcz7m8wwgqd0fdzpw-bash-4.4-p23.drv,[\"out\"]),(fhajhb521vwlcjhm5ymh3pyh7f0rd8ay-curl-7.74.0.drv,[\"dev\"]),(qgdxmika1avgrgxpdz1af8v953c1qgvh-stdenv-linux.drv,[\"out\"])]"
    env <- getDerivationEnv d
    env `shouldBe` M.fromList [("SSL_CERT_FILE", "/no-cert-file.crt"), ("buildInputs", ""), ("builder", "/nix/store/a3fc4zqaiak11jks9zd579mz5v0li8bg-bash-4.4-p23/bin/bash"), ("configureFlags", ""), ("curlOpts", ""), ("depsBuildBuild", ""), ("depsBuildBuildPropagated", ""), ("depsBuildTarget", ""), ("depsBuildTargetPropagated", ""), ("depsHostHost", ""), ("depsHostHostPropagated", ""), ("depsTargetTarget", ""), ("depsTargetTargetPropagated", ""), ("doCheck", ""), ("doInstallCheck", ""), ("downloadToTemp", ""), ("executable", ""), ("impureEnvVars", "http_proxy https_proxy ftp_proxy all_proxy no_proxy NIX_CURL_FLAGS NIX_HASHED_MIRRORS NIX_CONNECT_TIMEOUT NIX_MIRRORS_alsa NIX_MIRRORS_apache NIX_MIRRORS_bioc NIX_MIRRORS_bitlbee NIX_MIRRORS_centos NIX_MIRRORS_cpan NIX_MIRRORS_debian NIX_MIRRORS_fedora NIX_MIRRORS_gcc NIX_MIRRORS_gentoo NIX_MIRRORS_gnome NIX_MIRRORS_gnu NIX_MIRRORS_gnupg NIX_MIRRORS_hackage NIX_MIRRORS_hashedMirrors NIX_MIRRORS_imagemagick NIX_MIRRORS_kde NIX_MIRRORS_kernel NIX_MIRRORS_luarocks NIX_MIRRORS_maven NIX_MIRRORS_metalab NIX_MIRRORS_mozilla NIX_MIRRORS_mysql NIX_MIRRORS_oldsuse NIX_MIRRORS_openbsd NIX_MIRRORS_opensuse NIX_MIRRORS_osdn NIX_MIRRORS_postgresql NIX_MIRRORS_pypi NIX_MIRRORS_roy NIX_MIRRORS_sageupstream NIX_MIRRORS_samba NIX_MIRRORS_savannah NIX_MIRRORS_sourceforge NIX_MIRRORS_steamrt NIX_MIRRORS_ubuntu NIX_MIRRORS_xfce NIX_MIRRORS_xorg"), ("mirrorsFile", "/nix/store/mv7ky6l8q2zk8khp7xvmany2nb8phwi6-mirrors-list"), ("name", "hello-2.10.tar.gz"), ("nativeBuildInputs", "/nix/store/0bnxpjfknrwxi8kh9yb2js01xd6wz566-curl-7.74.0-dev"), ("nixpkgsVersion", "20.09"), ("out", "/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz"), ("outputHash", "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"), ("outputHashAlgo", "sha256"), ("outputHashMode", "flat"), ("outputs", "out"), ("patches", ""), ("postFetch", ""), ("preferHashedMirrors", "1"), ("preferLocalBuild", "1"), ("propagatedBuildInputs", ""), ("propagatedNativeBuildInputs", ""), ("showURLs", ""), ("stdenv", "/nix/store/av0pavd6vn698g82ml66gd2hnjd01nzb-stdenv-linux"), ("strictDeps", ""), ("system", "x86_64-linux"), ("urls", "mirror://gnu/hello/hello-2.10.tar.gz")]
    os <- getDerivationOutputs store drvName d
    show' os `shouldBe` "[DerivationOutput {derivationOutputName = \"out\", derivationOutputPath = Just 3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz, derivationOutputDetail = DerivationOutputCAFixed (FixedOutputHash Flat (Hash SHA256 \"1\\224f\\DC3z\\150&v\\232\\159i\\209\\182S\\130\\222\\149\\167\\239}\\145K\\140\\185V\\244\\RS\\167.\\SIQk\")) 3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz}]"

  it "parses a regular derivation correctly" $ withTempStore \store -> do
    bs <- BS.readFile "test/data/regular.drv"
    let drvName = "hello-2.10"
    d <- getDerivationFromString store drvName bs
    platform <- getDerivationPlatform d
    platform `shouldBe` "x86_64-linux"
    builder <- getDerivationBuilder d
    builder `shouldBe` "/nix/store/a3fc4zqaiak11jks9zd579mz5v0li8bg-bash-4.4-p23/bin/bash"
    arguments <- getDerivationArguments d
    arguments `shouldBe` ["-e", "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"]
    sources <- getDerivationSources store d
    show' sources `shouldBe` "[9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh]"
    inputs <- getDerivationInputs store d
    show' inputs `shouldBe` "[(7wbzr8nfcibnybvj38rrzkx8xzr7a1xl-stdenv-linux.drv,[\"out\"]),(abwabzabxq9vgd1rcz7m8wwgqd0fdzpw-bash-4.4-p23.drv,[\"out\"]),(w8z5d97dskfaj7wi12132pp7cn69f7xm-hello-2.10.tar.gz.drv,[\"out\"])]"
    env <- getDerivationEnv d
    env `shouldBe` M.fromList [("buildInputs", ""), ("builder", "/nix/store/a3fc4zqaiak11jks9zd579mz5v0li8bg-bash-4.4-p23/bin/bash"), ("configureFlags", ""), ("depsBuildBuild", ""), ("depsBuildBuildPropagated", ""), ("depsBuildTarget", ""), ("depsBuildTargetPropagated", ""), ("depsHostHost", ""), ("depsHostHostPropagated", ""), ("depsTargetTarget", ""), ("depsTargetTargetPropagated", ""), ("doCheck", "1"), ("doInstallCheck", ""), ("name", "hello-2.10"), ("nativeBuildInputs", ""), ("out", "/nix/store/8mygch54p7brcqn83xnfa22ik9y3km95-hello-2.10"), ("outputs", "out"), ("patches", ""), ("pname", "hello"), ("propagatedBuildInputs", ""), ("propagatedNativeBuildInputs", ""), ("src", "/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz"), ("stdenv", "/nix/store/f95jkch26wxmqd076bcm976qrn7ml46i-stdenv-linux"), ("strictDeps", ""), ("system", "x86_64-linux"), ("version", "2.10")]
    os <- getDerivationOutputs store drvName d
    show' os `shouldBe` "[DerivationOutput {derivationOutputName = \"out\", derivationOutputPath = Just 8mygch54p7brcqn83xnfa22ik9y3km95-hello-2.10, derivationOutputDetail = DerivationOutputInputAddressed 8mygch54p7brcqn83xnfa22ik9y3km95-hello-2.10}]"

show' :: Show a => a -> Text
show' = show
