module UtilSpec
  ( main
  , spec
  ) where

import           Pwn.Internal
import qualified System.Directory   as SD
import           System.Environment
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Util.getTemporaryDirectory" $ do
    it "use $XDG_RUNTIME_DIR if available" $ do
      setEnv "XDG_RUNTIME_DIR" "/path/to/tmpdir"
      tempdir1 <- getTemporaryDirectory
      tempdir1 `shouldBe` "/path/to/tmpdir"

      unsetEnv "XDG_RUNTIME_DIR"
      tempdir2 <- getTemporaryDirectory
      tempdir3 <- SD.getTemporaryDirectory
      tempdir2 `shouldBe` tempdir3
