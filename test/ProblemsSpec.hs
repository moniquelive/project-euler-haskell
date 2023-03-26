module ProblemsSpec where

import Problems.P1
import Problems.P10
import Problems.P11
import Problems.P12
import Problems.P13
import Problems.P14
import Problems.P15
import Problems.P16
import Problems.P17
import Problems.P18
import Problems.P19
import Problems.P2
import Problems.P20
import Problems.P21
import Problems.P22
import Problems.P23
import Problems.P24
import Problems.P25
import Problems.P26
import Problems.P27
import Problems.P28
import Problems.P29
import Problems.P3
import Problems.P30
import Problems.P31
import Problems.P34
import Problems.P35
import Problems.P36
import Problems.P38
import Problems.P4
import Problems.P40
import Problems.P41
import Problems.P48
import Problems.P5
import Problems.P50
import Problems.P52
import Problems.P6
import Problems.P67
import Problems.P7
import Problems.P8
import Problems.P9
import Test.Hspec

spec :: Spec
spec = do
  describe "all" $ do
    it "p50" $ do p50 `shouldBe` 997651
    it "p38" $ do p38 `shouldBe` 932718654
    it "p52" $ do p52 `shouldBe` 142857
    it "p41" $ do p41 `shouldReturn` 162
    it "p40" $ do p40 `shouldBe` 210
    it "p26" $ do p26 `shouldBe` 983
    it "p35" $ do p35 `shouldBe` 55
    it "p31" $ do p31 `shouldBe` 73682
    it "p27" $ do p27 `shouldBe` -59231
    it "p36" $ do p36 `shouldBe` 872187
    it "p34" $ do p34 `shouldBe` 40730
    it "p67" $ do p67 `shouldReturn` 7273
    it "p23" $ do p23 `shouldBe` 4179871
    it "p29" $ do p29 `shouldBe` 9183
    it "p28" $ do p28 `shouldBe` 669171001
    it "p30" $ do p30 `shouldBe` 443839
    it "p48" $ do p48 `shouldBe` "9110846700"
    it "p24" $ do p24 `shouldBe` "2783915460"
    it "p22" $ do p22 `shouldReturn` 871198282
    it "p19" $ do p19 `shouldBe` 171
    it "p18" $ do p18 `shouldReturn` 1074
    it "p21" $ do p21 `shouldBe` 31626
    it "p17" $ do p17 `shouldBe` 21124
    it "p1" $ do p1 `shouldBe` 233168
    it "p2" $ do p2 `shouldBe` 4613732
    it "p3" $ do p3 `shouldBe` 6857
    it "p6" $ do p6 `shouldBe` 25164150
    it "p5" $ do p5 `shouldBe` 232792560
    it "p4" $ do p4 `shouldBe` 906609
    it "p7" $ do p7 `shouldBe` 104743
    it "p9" $ do p9 `shouldBe` 31875000
    it "p10" $ do p10 `shouldBe` 142913828922
    it "p8" $ do p8 `shouldReturn` 23514624000
    it "p11" $ do p11 `shouldReturn` 70600674
    it "p16" $ do p16 `shouldBe` 1366
    it "p14" $ do p14 `shouldBe` 837799
    it "p13" $ do p13 `shouldReturn` 5537376230
    it "p12" $ do p12 `shouldBe` 76576500
    it "p20" $ do p20 `shouldBe` 648
    it "p15" $ do p15 `shouldBe` 137846528820
    it "p25" $ do p25 `shouldBe` 4782
