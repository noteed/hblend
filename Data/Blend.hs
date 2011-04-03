{-# LANGUAGE MagicHash, OverloadedStrings #-}
-- Blender's file format parsing library.
--
-- This library provides data structures to represent the main
-- parts of a blend-file, associated functions and Data.Binary parsers
-- to actually read a file into those data structures. Additionally
-- functions to generate Haskell code capable of a full parse of
-- particular blend-file versions are given. To make things simple,
-- such generated code for some versions of Blender are exposed.
--
-- Blender file format
--
-- A blend-file is the native binary format used by Blender to save
-- its data. At the most basic level a blend-file is just a header
-- followed by a list of blocks. Each file-block is itself made of a
-- header and some data. The block-header specifies what kind of data
-- is to be found after it.
--
-- To save its state, Blender dumps its internal data structures in
-- the data part of the file-blocks. Each block can contain multiple
-- structures, but all of the same type. Multiple blocks can contain
-- the same type of structure. The internal data structures are just C
-- structs.
--
-- Because each version of Blender can potentially use different data
-- structures, blend-files written by different versions of Blender can
-- contain different types of structures.
--
-- There is one special file-block, DNA1, whose data are a description
-- of all the data structures contained in the other file-blocks. That
-- description is called SDNA, for "Structure DNA". Each version of
-- Blender stores such SDNA in every written blend-files.
--
-- Usage of the library
--
-- This library can read a blend-file into a BBlend data structure.
-- A BBlend represents the main parts of blend-file: the header (in a
-- BHeader), the blocks (leaving their data unparsed, in BBlocks) and
-- the SDNA (this is the sole fully parsed block in a BBlend).
--
-- Using the SDNA, it is possible to generate code to represent and
-- read all the data part of the file-blocks. This library does that.
-- The approach is to refine the notion of BBlock into Block. The
-- generated code defines a variant data type called Block where each
-- variant correspond to a particular structure of the SDNA. This looks
-- like
--
-- data Block = 
--    BlockLink Integer [Link]
--  | BlockListBase Integer [ListBase]
--  | ...
--
-- The result of parsing the blend-file with the generated code is a list
-- of those Block. The Link, etc. data types are also generated. They look
-- like
--
-- data Link = Link
--  { link_next :: Integer {- struct Link * -}
--  , link_prev :: Integer {- struct Link * -}
--  }
--
-- Those data types are low-level: they correspond directly to the data
-- structures found in the blend-file. Further processing is left to you.
--
-- See http://www.blender.org/development/architecture/blender-file-format/
-- for some doc.

module Data.Blend where
--  (BHeader, BBlock, BBlend(..), SDNA(..), lookupStruct, showField, showStruct, readBlend, showStructAsHs, showStructParser, getPointer, getShort, getUShort, getInt, getLong, getULong, getFloat, getDouble)
--  where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Get (runGet)

import Data.List (find, intersperse)
import Data.Char (toLower, toUpper)

import Data.Blend.Types
import Data.Blend.Parser

showSDNAAsHs :: SDNA -> String
showSDNAAsHs sdna =
  "data Block = \n    " ++
  concat (intersperse "  | " $ map (f . fst) sdna) ++
  "  | BlockRaw Integer Struct Int ByteString -- for unparsed blocks.\n" ++
  "  deriving Show\n"
  where f n = "Block" ++ g n ++ " Integer [" ++ g n ++ "]\n"
        g n = toUpper (BC.head n) : tail (BC.unpack n)

showSDNAAsSDNA :: SDNA -> String
showSDNAAsSDNA sdna =
  "sdna :: SDNA\n" ++
  "sdna =\n  [ s" ++ concat (intersperse "\n  , s" $ map (BC.unpack . fst) sdna) ++ "\n  ]"

showBlockParser :: SDNA -> String
showBlockParser sdna =
  "readBlend :: FilePath -> IO [(Integer,Block)]\n" ++
  "readBlend f = do\n" ++
  "  s <- LB.readFile f\n" ++
  "  return $ runGet (do h <- getBHeader\n" ++
  "                      bs <- parseBlocks h\n" ++
  "                      return bs) s\n\n" ++
  "parseBlocks :: BHeader -> Get [(Integer,Block)]\n" ++
  "parseBlocks h = do\n" ++
  "  code <- getByteString 4\n" ++
  "  size <- fmap fromIntegral getWord32le\n" ++
  "  addr <- getAddress h\n" ++
  "  idx <- fmap fromIntegral getWord32le\n" ++
  "  count <- fmap fromIntegral getWord32le\n" ++
  "  case BC.unpack code of\n" ++
  "    \"DNA1\" -> return [] -- check SDNA is the same\n" ++
  "    \"ENDB\" -> return []\n" ++
  "    _ -> do b <- parseBlock h size addr idx count\n" ++
  "            bs <- parseBlocks h\n" ++
  "            return (b : bs)\n\n" ++
  "parseBlock :: BHeader -> Int -> Integer -> Int -> Int -> Get (Integer,Block)\n" ++
  "parseBlock h size addr idx count =\n" ++
  "  if structSize h (sdna !! idx) * count /= size\n" ++
  "  then do\n" ++
  "         dat <- getByteString size\n" ++
  "         return $ (addr, BlockRaw addr (sdna !! idx) count dat)\n" ++
  "  else case idx of\n" ++
  concat (zipWith f [(0::Integer)..] sdna) ++
  "         i -> error $ \"Unkown SDNA index \" ++ show i\n"
  where f i s = "         " ++ show i ++ " -> fmap (\\b -> (addr, Block" ++ g (fst s) ++ " addr b)) (replicateM count $ get" ++ g (fst s) ++ " h)\n"
        g n = toUpper (BC.head n) : tail (BC.unpack n)

-- Flattening of the SDNA (from the DNA1 block)

structFields :: Struct -> [Field]
structFields = snd

structSize :: BHeader -> Struct -> Int
structSize h = typeSize h . Compound

showStruct :: Struct -> String
showStruct (n, fs) = BC.unpack n ++ "\n" ++
  concatMap showField fs ++ "\n"

showStructAsHs :: Struct -> String
showStructAsHs (n, fs) =
  "data " ++ n' ++ " = " ++ n'
  ++ "\n  { " ++ map toLower (BC.unpack n) ++ "_" ++
  concat (intersperse ("  , "  ++ map toLower (BC.unpack n) ++ "_") $ map showFieldAsHs fs)
  ++ "  }\n  deriving Show\n"
  where n' = toUpper (BC.head n) : tail (BC.unpack n)

showStructAsStruct :: Struct -> String
showStructAsStruct (n, fs) =
  "s" ++ BC.unpack n ++ ":: Struct\n" ++
  "s" ++ BC.unpack n ++ " = (" ++ "\"" ++ BC.unpack n ++ "\",\n"
  ++ "  [ " ++
  concat (intersperse "  , " $ map showFieldAsField fs)
  ++ "  ])\n"

showStructParser :: Struct -> String
showStructParser (n, fs) =
  "get" ++ n' ++ " :: BHeader -> Get " ++ n' ++ "\n" ++
  "get" ++ n' ++ " h = do\n  " ++
  concat (intersperse "\n  "  f') ++
  "\n  return $ " ++ n'  ++ concatMap (\i -> " _" ++ show i) [1..length fs] ++
  "\n"
  where n' = toUpper (BC.head n) : tail (BC.unpack n)
        f =  map (showTypeParser . snd) fs
        f' = zipWith (\a b -> "_" ++ show a ++ " <- " ++ b) [(1::Integer)..] f

showFieldAsHs :: Field -> String
showFieldAsHs (n, t) = BC.unpack n ++ " :: " ++ showTypeAsHs t ++ "\n"

showFieldAsField :: Field -> String
showFieldAsField (n, t) = "(\"" ++ BC.unpack n ++ "\", " ++ showTypeAsType t ++ ")\n"

-- Used the nested comments syntax... so they can be nested.
showTypeAsHs :: Type -> String
showTypeAsHs  t =
  case t of 
    Char   -> "Int8 {- char -}"
    UChar  -> "Word8 {- uchar -}"
    Short  -> "Int16 {- short -}"
    UShort -> "Word16 {- ushort -}"
    Int    -> "Int32 {- int -}"
    Long   -> "Int64 {- long -}"
    ULong  -> "Word64 {- ulong -}"
    Float  -> "Float {- float -}"
    Double -> "Double {- double -}"
    Ref t' -> "Integer {- " ++ show t' ++ " * -}"
    RefVoid -> "Integer {- void * -}"
    Arr l Char -> "ByteString {- char[" ++ show l ++ "] -}"
    Arr l t' -> "[" ++ showTypeAsHs t' ++ "] {- " ++ show t' ++ "[" ++ show l ++ "] -}"
    FunPtr t' -> "Integer {- " ++ show t' ++ " (*xxx)() -}"
    Compound s -> let n = toUpper (BC.head $ structName s) :
                          tail (BC.unpack $ structName s)
                  in n ++ " {- struct " ++ BC.unpack (structName s) ++ " -}"
    UnknownCompound n -> "UnknownCompound (\"" ++ BC.unpack n ++ "\")"

showTypeAsType :: Type -> String
showTypeAsType  t =
  case t of 
    Char   -> "Char"
    UChar  -> "UChar"
    Short  -> "Short"
    UShort -> "UShort"
    Int    -> "Int"
    Long   -> "Long"
    ULong  -> "ULong"
    Float  -> "Float"
    Double -> "Double"
    Ref (UnknownCompound n) -> "RefVoid {- Ref (UnknownCompound " ++ BC.unpack n ++ ") -}"
    Ref t' -> "Ref (" ++ showTypeAsType t' ++ ")"
    RefVoid -> "RefVoid"
    Arr l t' -> "Arr " ++ show l ++ " (" ++ showTypeAsType t' ++ ")"
    FunPtr t' -> "FunPtr (" ++ showTypeAsType  t' ++ ")"
    Compound s -> "Compound s" ++ BC.unpack (structName s)
    UnknownCompound n -> "UnknownCompound (\"" ++ BC.unpack n ++ "\")"

showTypeParser :: Type -> String
showTypeParser  t =
  case t of 
    Char   -> "get {- char -}"
    UChar  -> "get {- uchar -}"
    Short  -> "getShort h {- short -}"
    UShort -> "getUshort h {- ushort -}"
    Int    -> "getInt h {- int -}"
    Long   -> "getLong h {- long -}"
    ULong  -> "getULong h {- ulong -}"
    Float  -> "getFloat h {- float -}"
    Double -> "getDouble h {- double -}"
    Ref (UnknownCompound n) -> "getPointer h {- UnknownCompound " ++ BC.unpack n ++ "  -}"
    Ref t' -> "getPointer h {- " ++ show t' ++ " * -}"
    RefVoid -> "getPointer h {- void * -}"
    Arr l Char -> "getByteString " ++ show l ++ " {- char[" ++ show l ++ "] -}"
    Arr l t' -> "replicateM " ++ show l ++" (" ++ showTypeParser t' ++ ") {- " ++ show t' ++ "[" ++ show l ++ "] -}"
    FunPtr t' -> "getPointer h {- " ++ show t' ++ " (*xxx)() -}"
    Compound s -> let n = toUpper (BC.head $ structName s) :
                          tail (BC.unpack $ structName s)
                  in "get" ++ n ++ " h {- struct " ++ BC.unpack (structName s) ++ " -}"
    UnknownCompound n -> error $ "Can't generate parsing code for UnknownCompound " ++ BC.unpack n

-- Reads a blend-file into the basic BBlend data structures.
readBBlend :: FilePath -> IO BBlend
readBBlend f = do
  s <- LB.readFile f
  return $ runGet getBBlend s

-- Reads just a blend-file header.
readBHeader :: FilePath -> IO BHeader
readBHeader f = do
  s <- LB.readFile f
  return $ runGet getBHeader s

-- Convenience functions 

lookupStruct :: BBlend -> String -> Maybe Struct
lookupStruct (BBlend _ _ sdna) n =
  fmap (\s -> (BC.pack n,s)) $ lookup (BC.pack n) sdna

lookupStructByIndex :: BBlend -> Int -> Struct
lookupStructByIndex (BBlend _ _ sdna) = (sdna !!)

lookupBlockByOldAddr :: BBlend -> Integer -> Maybe BBlock
lookupBlockByOldAddr bf addr = find f (blendBlocks bf)
  where f b = oldAddr b == addr

-- Checks if a given block has a specific code and SDNA structure.
hasCodeAndSDNA :: BBlock -> String -> String -> Bool
hasCodeAndSDNA b c s = code && struct
  where code = blockCode b == BC.pack c
        struct = fst (sdnaStruct b) == BC.pack s

