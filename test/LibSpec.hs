module LibSpec where

import Lib

import Test.Hspec

spec :: Spec
spec = do
    describe "all" $ do
        it "validates p21" $ do
            p21 `shouldBe` 31626

        it "validates p17" $ do
            p17 `shouldBe` 21124

        it "validates p25" $ do
            p25 `shouldBe` 4782

        it "validates p1" $ do
            p1 `shouldBe` 233168

        it "validates p2" $ do
            p2 `shouldBe` 4613732

        it "validates p3" $ do
            p3 `shouldBe` 6857

        it "validates p6" $ do
            p6 `shouldBe` 25164150

        it "validates p5" $ do
            p5 `shouldBe` 232792560

        it "validates p4" $ do
            p4 `shouldBe` 906609

        it "validates p7" $ do
            p7 `shouldBe` 104743

        it "validates p9" $ do
            p9 `shouldBe` 31875000

        it "validates p10" $ do
            p10 `shouldBe` 142913828922

-- -- 23514624000
-- p8 :: IO Int

        it "validates p11" $ do
            p11 `shouldBe` 70600674

        it "validates p16" $ do
            p16 `shouldBe` 1366

        it "validates p14" $ do
            p14 `shouldBe` 837799

        it "validates p13" $ do
            p13 `shouldBe` 5537376230

        it "validates p12" $ do
            p12 `shouldBe` 76576500

        it "validates p20" $ do
            p20 `shouldBe` 648

        it "validates p15" $ do
            p15 `shouldBe` 137846528820
