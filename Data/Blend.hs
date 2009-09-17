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

import Data.Int
import Foreign.C.Types (CDouble, CFloat)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts hiding (FunPtr)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString, pack)

import Data.Binary
import Data.Binary.Get (bytesRead, getByteString, getWord64be, getWord32be, getWord16be,  getWord64le, getWord32le, getWord16le, skip, runGet)

import Control.Monad (when, replicateM)

import Data.List (find, intersperse)
import Data.Char (toLower, toUpper)

-- .blend file header

-- The identifier is always "BLENDER" and is not part of the structure.
-- The version number should be 3 bytes long.
data BHeader = BHeader
  { pointerSize   :: PointerSize
  , endianness     :: Endianness
  , versionNumber :: ByteString
  }

data PointerSize = Pointer32 | Pointer64

data Endianness = LittleEndian | BigEndian

instance Show BHeader where
  show (BHeader ps en v) =
    let (ps',ps'') = case ps of
          { Pointer32 -> ("_","32") ; _ -> ("-","64") }
        (en',en'') = case en of
          { LittleEndian -> ("v","little") ; _ -> ("V","big") }
    in "File header: BLENDER" ++ ps' ++ en' ++ BC.unpack v
       ++ " (" ++ ps'' ++ " bits pointers, " ++ en'' ++ " endian)\n"

getBHeader :: Get BHeader
getBHeader = do
  identifier <- getByteString 7
  when (identifier /= "BLENDER") $ error "No \
\\"BLENDER\" file identifier in the 7 first bytes."
  ps <- get
  let ps' = case ps of
              '_' -> Pointer32
              '-' -> Pointer64
              _ -> error "Underscore or minus character \
\expected after \"BLENDER\" file identifer."
  en <- get
  let en' = case en of
              'v' -> LittleEndian
              'V' -> BigEndian
              _ -> error "V or v character expected after \
\\"BLENDER\" file identifer."
  v <- getByteString 3
  return $ BHeader ps' en' v

-- File block header and data
-- The old address is an Integer, enough for 64 bits pointers.
-- The effective pointer size is given by the file header (BHeader).

data BBlock = BBlock
  { -- header part
    blockCode   :: ByteString -- 4 bytes, identifier of the block
  , oldAddr     :: Integer    -- memory address when the structure was written to disk
  , sdnaStruct  :: Struct     -- SDNA structure index
  , structCount :: Int        -- number of SDNA structures in the file-block-data
    -- data part
  , blockData   :: ByteString
  }

-- Use is the 'down' pass then converted to BBlock in the
-- 'up' pass, see getBBlocks.
data Block = Block
  { -- header part
    blckCode         :: ByteString -- 4 bytes, identifier of the block
  , blckOldAddr     :: Integer    -- memory address when the structure was written to disk
  , sdnaIndex        :: Int        -- SDNA structure index
  , blckStructCount :: Int        -- number of SDNA structures in the file-block-data
    -- data part
  , blckData         :: ByteString
  }

instance Show BBlock where
  show (BBlock code _ s count dat) =
    let siz = typeSize (BHeader Pointer32 LittleEndian "248") (Compound s) in
    "File block header: " ++ BC.unpack code ++ ", " ++ show (BC.length dat) ++
    " bytes for " ++ show count ++ " " ++ BC.unpack (fst s) ++ "\n"
    ++ show siz
    ++ " * " ++ show count ++ " = " ++ show (count * siz) ++ "\n"

-- Get a file-block.
-- The data part of the file-block is left as a raw ByteString at this stage.
getBlock :: BHeader -> Get Block
getBlock h = do
  code <- getByteString 4
  size <- fmap fromIntegral getWord32le
  addr <- getAddress h
  idx <- fmap fromIntegral getWord32le
  count <- fmap fromIntegral getWord32le
  dat <- getByteString size
  return $ Block code addr idx count dat

-- DNA1 block

type SDNA = [Struct]

-- The number of items (fields) is not
-- explicitely needed as we can query the length of a list or an array.
-- Instead of indices for the types and names, we store directly the
-- types and the names.
--                       name,        (type, name)
type SDNAStruct = (ByteString, [(ByteString, ByteString)])

getSDNA :: Get SDNA
getSDNA = do
  skipAndCheck "SDNA"
  skipAndCheck "NAME"
  nc <- fmap fromIntegral getWord32le
  ns <- getNulTerminatedStrings (nc::Integer)
  align 4
  skipAndCheck "TYPE"
  tc <- fmap fromIntegral getWord32le
  ts <- getNulTerminatedStrings tc
  align 4
  skipAndCheck "TLEN"
  replicateM tc get :: Get [Int16]
  align 4
  skipAndCheck "STRC"
  sc <- fmap fromIntegral getWord32le
  ss <- replicateM sc (getSDNAStruct ns ts)
  return (flatten ss)

getTypeAndName :: [ByteString] -> [ByteString] -> Get (ByteString,ByteString)
getTypeAndName ns ts = do
  a <- fmap ((ts !!) . fromIntegral) getWord16le
  b <- fmap ((ns !!) . fromIntegral) getWord16le
  return (a,b)

-- Parses the name and the fields of a structure as ByteStrings
-- (not as Int16 indices). The list of names and types are passed
-- as arguments to make the lookup of the ByteStrings possible.
getSDNAStruct :: [ByteString] -> [ByteString] -> Get SDNAStruct
getSDNAStruct ns ts = do
  st <- fmap ((ts !!) . fromIntegral) getWord16le
  fc <- fmap fromIntegral getWord16le
  fs <- replicateM fc (getTypeAndName ns ts)
  return (st, fs)

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
  "sdna =\n  [ s" ++ concat (intersperse "\n  , s" $ map (BC.unpack . fst) sdna) ++ "\n  ]"

showBlockParser :: SDNA -> String
showBlockParser sdna =
  "readBlend f = do\n" ++
  "  s <- LB.readFile f\n" ++
  "  return $ runGet (do h <- getBHeader\n" ++
  "                      bs <- parseBlocks h\n" ++
  "                      return bs) s\n" ++
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
  "            return (b : bs)\n" ++
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

-- Flattened version of SDNAStruct (ByteStrings are
-- dereferenced and replaced by the Types they name).
type Struct = (ByteString, [Field])

structName :: Struct -> ByteString
structName = fst

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
  "s" ++ BC.unpack n ++ " = (" ++ "\"" ++ BC.unpack n ++ "\",\n"
  ++ "  [ " ++
  concat (intersperse "  , " $ map showFieldAsField fs)
  ++ "  ])\n"

showStructAsHtml :: (Integral i) => i -> Struct -> String
showStructAsHtml i (n, fs) =
  "<div class=\"sdna-structure\"><span class=\"sdna-index\">"
  ++ show i ++ "</span><span class=\"sdna-structure-name\">"
  ++ " structure " ++ BC.unpack n ++ "</span>\n" ++
  concat (intersperse "  , "  $ map showFieldAsHtml fs)
  ++ "</div>\n"

showStructParser :: Struct -> String
showStructParser (n, fs) =
  "get" ++ n' ++ " h = do\n  " ++
  concat (intersperse "\n  "  f') ++
  "\n  return $ " ++ n'  ++ concatMap (\i -> " _" ++ show i) [1..length fs]
  where n' = toUpper (BC.head n) : tail (BC.unpack n)
        f =  map (showTypeParser . snd) fs
        f' = zipWith (\a b -> "_" ++ show a ++ " <- " ++ b) [(1::Integer)..] f

type Field = (ByteString, Type)

showField :: Field -> String
showField (n, t) = BC.unpack n ++ " :: " ++ show t ++ "\n"

showFieldAsHs :: Field -> String
showFieldAsHs (n, t) = BC.unpack n ++ " :: " ++ showTypeAsHs t ++ "\n"

showFieldAsField :: Field -> String
showFieldAsField (n, t) = "(\"" ++ BC.unpack n ++ "\", " ++ showTypeAsType t ++ ")\n"

showFieldAsHtml :: Field -> String
showFieldAsHtml (n, t) =
  "<span class=\"sdna-field\"><span class=\"sdna-field-type\">"
  ++ showTypeAsHtml t ++ "</span><span class=\"sdna-field-name\">"
  ++ BC.unpack n ++ "</span></span>\n"

data Type = Char | UChar
          | Short | UShort
          | Int
          | Long | ULong
          | Float | Double
          | Ref Type
          | RefVoid
          | Arr Int Type
          | FunPtr Type -- return type, not arguments
          | Compound Struct
          | UnknownCompound ByteString

instance Show Type where
  show  t =
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
      UnknownCompound n -> "struct " ++ BC.unpack n ++ " /* undefined */"

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

-- Given flattened Structs, flatten another struct.
flattenStruct :: [Struct] -> SDNAStruct -> Struct
flattenStruct ss (n,fs) = (n, (map (flattenField ss) fs))

flattenField :: [Struct] -> (ByteString, ByteString) -> Field
flattenField ss (t,n) = (n',t''')
  where -- type unaffected by the name
        t' = case BC.unpack t of
               "char" -> Char
               "uchar" -> UChar
               "short" ->  Short
               "ushort" -> UShort
               "int" -> Int
               "long" -> Long
               "ulong" -> ULong
               "float" -> Float
               "double" -> Double
               "void" -> RefVoid -- (BC.head n) should be '*'
               _ -> case lookup (name t) ss of
                      Nothing -> UnknownCompound (name t)
                      Just fs -> Compound (name t, fs)
        name = BC.takeWhile (/= '[') . BC.dropWhile (== '*')
        star = BC.length . BC.takeWhile (== '*')
        -- type refined by the name (pointer and/or array)
        t'' = case (star n, lengths n) of
                (0, []) -> t'
                (1, []) -> Ref t'
                (2, []) -> Ref (Ref t')
                (0, [l]) -> Arr l t'
                (1, [l]) -> Arr l (Ref t')
                (0, [l,m]) -> Arr m (Arr l t')
                (1, [l,m]) -> Arr m (Arr l (Ref t'))
                _ -> error "Unexpected (type,name)"
        -- the name can be (*xxx)()
        t''' = if BC.head (name n) == '(' then FunPtr t'' else t''
        n' = if BC.head (name n) == '(' then BC.takeWhile (/= ')') (BC.drop 2 n) else name n

lengths :: ByteString -> [Int]
lengths s =
  let s' = BC.drop 1 (BC.dropWhile (/= '[') s)
  in case BC.readInt s' of
       Just (i, s'') -> i : lengths s''
       Nothing -> []

flatten :: [SDNAStruct] -> [Struct]
flatten sdnaStructs = let ss = map (flattenStruct ss) sdnaStructs in ss

typeSize :: BHeader -> Type -> Int
typeSize h t = 
  case t of 
    Char -> 1
    UChar -> 1
    Short -> 2
    UShort -> 2
    Int -> 4
    Long -> 8
    ULong -> 8
    Float -> 4
    Double -> 8
    Ref (UnknownCompound _) -> ps
    Ref _ -> ps
    RefVoid -> ps
    Arr l t' -> typeSize h t' * l
    FunPtr _ -> ps -- Function pointer size assumed to be pointer size
    Compound s -> sum (map (typeSize h . snd) (snd s))
    UnknownCompound n -> error $ "Can't compute the size fo UnknownCompound " ++ BC.unpack n
  where ps = case (pointerSize h) of
               Pointer32 -> 4
               Pointer64 -> 8

----------------------------------------------------------------------
-- Low level blend-file: the block data are left as ByteStrings
----------------------------------------------------------------------

data BBlend = BBlend
  { blendHeader :: BHeader
  , blendBlocks :: [BBlock]
  , blendSdna   :: SDNA
  }

instance Show BBlend where
  show (BBlend h bs sdna) =
    show h ++ "\n" ++ concatMap show bs ++ "\n" ++ show sdna ++ "\n"

-- Parses a list of file-blocks. It stops parsing when it reads
-- the "ENDB" block (which is not returned). It completes the
-- given blend-file.
--
-- Getting the blocks is done recursively. The recursion is used to
-- parse the whole file in two passes : when going down the recursion
-- then when going up. The SDNA should be available when going up so
-- indices into the SDNA are directly resolved and replaced by the
-- corresponding structure description.
getBlocks :: BBlend -> Get BBlend
getBlocks bf = do
  b <- getBlock (blendHeader bf)
  case BC.unpack (blckCode b) of
    "DNA1" -> do bf' <- getBlocks bf
                 let sdna = runGet getSDNA $ LB.fromChunks [blckData b]
                 return $ bf' { blendSdna = sdna }
    "ENDB" -> return bf
    _ -> do bf' <- getBlocks bf
            let (Block c a i n d) = b
                sdna = blendSdna bf'
                s = sdna !! i
                b' = BBlock c a s n d
            return $ bf' { blendBlocks = b' : blendBlocks bf' }

getBBlend :: Get BBlend
getBBlend = do
  h <- getBHeader
  getBlocks (BBlend h [] [])

-- Reads a blend-file into the basic BBlend data structures.
readBBlend :: FilePath -> IO BBlend
readBBlend f = do
  s <- LB.readFile f
  return $ runGet getBBlend s

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

----------------------------------------------------------------------
-- Parsing primitives.
-- The blend-file header is given in argument to account for
-- the endiannes and the pointer-sizeused to write the file.
-- The size of a function pointer is assumed to be the size of
-- a regular pointer.
----------------------------------------------------------------------

-- Returns an address according to the endianness an
-- pointer size of a given header.
getAddress :: BHeader -> Get Integer
getAddress h =
  case (endianness h, pointerSize h) of
    (LittleEndian, Pointer32) -> fmap fromIntegral getWord32le
    (LittleEndian, Pointer64) -> fmap fromIntegral getWord64le
    (BigEndian, Pointer32) -> fmap fromIntegral getWord32be
    (BigEndian, Pointer64) -> fmap fromIntegral getWord64be

getPointer :: BHeader -> Get Integer
getPointer = getAddress

getShort :: BHeader -> Get Int16
getShort h =
  case endianness h of
    LittleEndian -> fmap fromIntegral getWord16le
    BigEndian -> fmap fromIntegral getWord16be

getUShort :: BHeader -> Get Word16
getUShort h =
  case endianness h of
    LittleEndian -> fmap fromIntegral getWord16le
    BigEndian -> fmap fromIntegral getWord16be

getInt :: BHeader -> Get Int32
getInt h =
  case endianness h of
    LittleEndian -> fmap fromIntegral getWord32le
    BigEndian -> fmap fromIntegral getWord32be

getLong :: BHeader -> Get Int64
getLong h =
  case endianness h of
    LittleEndian -> fmap fromIntegral getWord64le
    BigEndian -> fmap fromIntegral getWord64be

getULong :: BHeader -> Get Word64
getULong h =
  case endianness h of
    LittleEndian -> fmap fromIntegral getWord64le
    BigEndian -> fmap fromIntegral getWord64be

-- Thanks to Judah Jacobson on the Haskell mailing-list for the
-- Word32/Float and Word64/Double conversions.

getFloat :: BHeader -> Get Float
getFloat h = fmap coerce (
 case endianness h of
   LittleEndian -> getWord32le
   BigEndian -> getWord32be
 )
 where
   coerce w = unsafePerformIO $ with w $ \p -> do
                d <- peek (castPtr p) :: IO CFloat
                return (realToFrac d :: Float)

getDouble :: BHeader -> Get Double
getDouble h = fmap coerce (
 case endianness h of
   LittleEndian -> getWord64le
   BigEndian -> getWord64be
 )
 where
   coerce w = unsafePerformIO $ with w $ \p -> do
                d <- peek (castPtr p) :: IO CDouble
                return (realToFrac d :: Double)

skipAddress :: BHeader -> Get ()
skipAddress h =
  case pointerSize h of
    Pointer32 -> skip 4
    Pointer64 -> skip 8

skipAndCheck :: ByteString -> Get ()
skipAndCheck s = do
  b <- getByteString (BC.length s)
  when (b /= s) $ error $ "Expected " ++ BC.unpack s ++
    ", read " ++ BC.unpack b ++ ".\n"

-- Parses a null-terminated string (doesn't return the null byte).
getNulTerminatedString :: Get ByteString
getNulTerminatedString = fmap pack go
  where go :: Get [Word8]
        go = do w <- get
                if w == 0
                  then return []
                  else do ws <- go
                          return (w : ws)

-- Parses n null-terminated strings.
getNulTerminatedStrings :: (Integral a) => a -> Get [ByteString]
getNulTerminatedStrings n =
  replicateM (fromIntegral n) getNulTerminatedString

-- Skips bytes if necessary to ensure proper parsing of aligned data.
align :: Int64 -> Get ()
align a = do
  n <- bytesRead
  case n `mod` a of
    0 -> return ()
    r -> skip (fromIntegral $ a - r)

