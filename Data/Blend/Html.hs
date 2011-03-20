module Data.Blend.Html where

import qualified Data.ByteString.Char8 as BC
import Data.List (intersperse)

import Data.Blend.Types

-- .blend file header

showStructAsHtml :: (Integral i) => i -> Struct -> String
showStructAsHtml i (n, fs) =
  "<div class=\"sdna-structure\"><span class=\"sdna-index\">"
  ++ show i ++ "</span><span class=\"sdna-structure-name\">"
  ++ " structure " ++ BC.unpack n ++ "</span>\n" ++
  concat (intersperse "  , "  $ map showFieldAsHtml fs)
  ++ "</div>\n"

showFieldAsHtml :: Field -> String
showFieldAsHtml (n, t) =
  "<span class=\"sdna-field\"><span class=\"sdna-field-type\">"
  ++ showTypeAsHtml t ++ "</span><span class=\"sdna-field-name\">"
  ++ BC.unpack n ++ "</span></span>\n"

showTypeAsHtml :: Type -> String
showTypeAsHtml t =
  case t of 
    Char -> "char"
    UChar -> "uchar"
    Short -> "short"
    UShort -> "ushort"
    Int -> "int"
    Long -> "long"
    ULong -> "ulong"
    Float -> "float"
    Double -> "double"
    Ref t' -> show t' ++ " *"
    RefVoid -> "void *"
    Arr l t' -> show t' ++ " [" ++ show l ++ "]"
    FunPtr t' -> show t' ++ "(*xxx)()"
    Compound s -> "struct " ++ BC.unpack (structName s)
    UnknownCompound n -> "struct " ++ BC.unpack n ++ " (undefined)"
