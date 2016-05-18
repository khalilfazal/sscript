module Char.SScriptSpec where

import Char.SScript

import Control.Monad (ap)
import Test.Hspec

shouldBeIdempotent :: (Eq a, Show a) => (a -> a) -> a -> Expectation
shouldBeIdempotent = ap shouldBe

convertDecimals :: (Char -> Char) -> String
convertDecimals f = (map f . concatMap show) [0 .. 9]

symbols :: String
symbols = "+-=()"

spec :: Spec
spec = do
    context "Char.SScript.subscript" $ do
        it "should work for a single char" $
            subscript '0' `shouldBe` '₀'
        it "should work for all single digit decimals" $
            convertDecimals subscript `shouldBe` "₀₁₂₃₄₅₆₇₈₉"
        it "should work for +-=() symbols" $
            map subscript symbols `shouldBe` "₊₋₌₍₎"
        it "should work for selected letters" $
            map subscript "aeioruvx" `shouldBe` "ₐₑᵢₒᵣᵤᵥₓ"
        it "should return the same char if it can't be subscripted" $
            shouldBeIdempotent (map subscript) "AEIORUVX"

    context "Char.SScript.superscript" $ do
        it "should work for a single char" $
            superscript '0' `shouldBe` '⁰'
        it "should work for all single digit decimals" $
            convertDecimals superscript `shouldBe` "⁰¹²³⁴⁵⁶⁷⁸⁹"
        it "should work for +-=() symbols" $
            map superscript symbols `shouldBe` "⁺⁻⁼⁽⁾"
        it "should work for all letters except qCFQSVXYZ" $
            map superscript "abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
            `shouldBe`
            "ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"
        it "should return the same char if it can't be superscripted" $
            shouldBeIdempotent (map superscript) "qCFQSVXYZ"

    context "Char.SScript.formatSS" $ do
        it "should convert chars following an underscore to its subscript, \
            \like in the chemical formula for dravite" $
            formatSS "NaMg_{3}(Al,Mg)_6B_3Si_6O_{27}(OH)"
            `shouldBe`
            "NaMg₃(Al,Mg)₆B₃Si₆O₂₇(OH)"
        it "should convert chars following a caret to its superscript" $
            formatSS "(a^n)^{r+s}" `shouldBe` "(aⁿ)ʳ⁺ˢ"
        it "should convert strings containing a mixture of underscores and carets" $
            formatSS "(x_{12} - x_{21})^{25} + (y_1 - y_2)^2"
            `shouldBe`
            "(x₁₂ - x₂₁)²⁵ + (y₁ - y₂)²"
