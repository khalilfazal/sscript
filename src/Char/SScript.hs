module Char.SScript (
    subscript,
    superscript,
    formatSS
) where

import Control.Monad (ap)
import Data.Maybe    (fromMaybe)

convertOrId :: Eq a => [(a, a)] -> a -> a
convertOrId = ap fromMaybe . flip lookup

subscripts :: [(Char, Char)]
subscripts =
    zip "0123456789+-=()aeioruvx"
        "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑᵢₒᵣᵤᵥₓ"

subscript :: Char -> Char
subscript = convertOrId subscripts

superscripts :: [(Char, Char)]
superscripts =
    zip "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
        "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"

superscript :: Char -> Char
superscript = convertOrId superscripts

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
