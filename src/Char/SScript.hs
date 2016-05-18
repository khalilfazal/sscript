-- |
-- Module      : sscript
-- License     : BSD3
-- Maintainer  : khalil.fazal@uoit.net
-- Portability : portable
--
-- Formats Strings with superscript or subscript characters
module Char.SScript (
    formatSS,
    subscript,
    superscript
) where

import Control.Monad (ap)
import Data.Maybe    (fromMaybe)

convertOrId :: Eq a => [(a, a)] -> a -> a
convertOrId = ap fromMaybe . flip lookup

subscripts :: [(Char, Char)]
subscripts =
    zip "0123456789+-=()aeioruvx"
        "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑᵢₒᵣᵤᵥₓ"

-- | subscripts a char
-- > subscript '0' == '₀'
subscript :: Char -> Char
subscript = convertOrId subscripts

superscripts :: [(Char, Char)]
superscripts =
    zip "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
        "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"

-- | superscripts a char
-- > subscript '0' == '⁰'
superscript :: Char -> Char
superscript = convertOrId superscripts

-- | formats a string
-- > formatSS "x_1^2 + x_2^2 + x_3^2 = z^2" == "x₁² + x₂² + x₃² = z²"
formatSS :: String -> String
formatSS ('^' : '{' : xs) = formatUntilBrace superscript xs
formatSS ('_' : '{' : xs) = formatUntilBrace subscript   xs
formatSS ('^' :  x  : xs) = superscript x : formatSS xs
formatSS ('_' :  x  : xs) = subscript   x : formatSS xs
formatSS (x         : xs) = x : formatSS xs
formatSS []               = []

formatUntilBrace :: (Char -> Char) -> String -> String
formatUntilBrace ss xs = map ss x ++ formatSS ys
    where
        (x, _ : ys) = break (== '}') xs
