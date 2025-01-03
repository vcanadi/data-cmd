module DataCmdSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "test title" $
    it "description" $ True == True
