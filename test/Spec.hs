main :: IO ()
main = putStrLn "Test suite not yet implemented"

packageVersions :: Spec ()
packageVersions =
    describe "Package version parsing handles" $ do
        it "packages with only one number" $ do
            let pkg = "1f779011e9986a290dc252dce85d9a162968d5d5:pkgs/development/compilers/ghc/8.2.1.nix:  version = \"1\";"
            return ()
        it "packages with numbers and dots" $ do
            let pkg = "2b1eabc14507be020d52c2ecb08da7a0f217a82f:pkgs/development/compilers/purescript/purescript/default.nix:  version = \"0.13.0\";"
            return ()
        it "packages with letters before numbers" $ do
            let pkg = "46a181a602e6e036736dbf3171fbd0c2e2df982b:pkgs/development/compilers/purescript/purescript/default.nix:  version = \"v0.12.4\";"
        it "packages with letters, numbers and dashes"
            let pkg = "1f779011e9986a290dc252dce85d9a162968d5d5:pkgs/development/compilers/ghc/8.2.1.nix:  version = \"8.2.1-rc3\";"
