module LibSpec where

import Lib

import Test.Hspec

spec :: Spec
spec = do
    describe "all" $ do
        it "validates p40" $ do
            p40 `shouldBe` 210

        it "validates p26" $ do
            p26 `shouldBe` 983

        it "validates p35" $ do
            p35 `shouldBe` 55

        it "validates p31" $ do
            p31 `shouldBe` 73682

        it "validates p27" $ do
            p27 `shouldBe` -59231

        it "validates p36" $ do
            p36 `shouldBe` 872187

        it "validates p34" $ do
            p34 `shouldBe` 40730

        it "validates p67" $ do
            p67 `shouldReturn` 7273

        it "validates p23" $ do
            p23 `shouldBe` 4179871

        it "validates p29" $ do
            p29 `shouldBe` 9183

        it "validates p28" $ do
            p28 `shouldBe` 669171001

        it "validates p30" $ do
            p30 `shouldBe` 443839

        it "validates p48" $ do
            p48 `shouldBe` "9110846700"

        it "validates p24" $ do
            p24 `shouldBe` "2783915460"

        it "validates p22" $ do
            p22 `shouldReturn` 871198282

        it "validates p19" $ do
            p19 `shouldBe` 171

        it "validates p18" $ do
            p18 `shouldReturn` 1074

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

        it "validates p8" $ do
            p8 `shouldReturn` 23514624000

        it "validates p11" $ do
            p11 `shouldReturn` 70600674

        it "validates p16" $ do
            p16 `shouldBe` 1366

        it "validates p14" $ do
            p14 `shouldBe` 837799

        it "validates p13" $ do
            p13 `shouldReturn` 5537376230

        it "validates p12" $ do
            p12 `shouldBe` 76576500

        it "validates p20" $ do
            p20 `shouldBe` 648

        it "validates p15" $ do
            p15 `shouldBe` 137846528820
