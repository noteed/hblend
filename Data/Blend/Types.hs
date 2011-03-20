{-# LANGUAGE OverloadedStrings #-}
module Data.Blend.Types where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

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

-- Used in the 'down' pass then converted to BBlock in the
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

-- DNA1 block

type SDNA = [Struct]

-- The number of items (fields) is not
-- explicitely needed as we can query the length of a list or an array.
-- Instead of indices for the types and names, we store directly the
-- types and the names.
--                       name,        (type, name)
type SDNAStruct = (ByteString, [(ByteString, ByteString)])

-- Flattened version of SDNAStruct (ByteStrings are
-- dereferenced and replaced by the Types they name).
type Struct = (ByteString, [Field])

structName :: Struct -> ByteString
structName = fst

type Field = (ByteString, Type)

showField :: Field -> String
showField (n, t) = BC.unpack n ++ " :: " ++ show t ++ "\n"

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
