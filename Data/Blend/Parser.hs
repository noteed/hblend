{-# LANGUAGE OverloadedStrings #-}
module Data.Blend.Parser where

import Data.Int
import Foreign.C.Types (CDouble, CFloat)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString, pack)

import Data.Binary
import Data.Binary.Get
  ( bytesRead, getByteString
  , getWord64be, getWord32be, getWord16be
  , getWord64le, getWord32le, getWord16le
  , skip, runGet
  )

import Control.Monad (when, replicateM)

import Data.Blend.Types

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

-- Get a file-block.
-- The data part of the file-block is left as a raw ByteString at this stage.
getBlock :: BHeader -> Get Block
getBlock h = do
  code <- getByteString 4
  size <- fromIntegral <$> getWord32 h
  addr <- getAddress h
  idx <- fromIntegral <$> getWord32 h
  count <- fromIntegral <$> getWord32 h
  dat <- getByteString size
  return $ Block code addr idx count dat

getSDNA :: BHeader -> Get SDNA
getSDNA h = do
  skipAndCheck "SDNA"
  skipAndCheck "NAME"
  nc <- fromIntegral <$> getWord32 h
  ns <- getNulTerminatedStrings (nc::Integer)
  align 4
  skipAndCheck "TYPE"
  tc <- fromIntegral <$> getWord32 h
  ts <- getNulTerminatedStrings tc
  align 4
  skipAndCheck "TLEN"
  _ <- replicateM tc get :: Get [Int16]
  align 4
  skipAndCheck "STRC"
  sc <- fromIntegral <$> getWord32 h
  ss <- replicateM sc (getSDNAStruct h ns ts)
  return (flatten ss)

getTypeAndName :: BHeader -> [ByteString] -> [ByteString] -> Get (ByteString,ByteString)
getTypeAndName h ns ts = do
  a <- ((ts !!) . fromIntegral) <$> getWord16 h
  b <- ((ns !!) . fromIntegral) <$> getWord16 h
  return (a,b)

-- Parses the name and the fields of a structure as ByteStrings
-- (not as Int16 indices). The list of names and types are passed
-- as arguments to make the lookup of the ByteStrings possible.
getSDNAStruct :: BHeader -> [ByteString] -> [ByteString] -> Get SDNAStruct
getSDNAStruct h ns ts = do
  st <- ((ts !!) . fromIntegral) <$> getWord16 h
  fc <- fromIntegral <$> getWord16 h
  fs <- replicateM fc (getTypeAndName h ns ts)
  return (st, fs)

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
        (n', t''') = if not (BC.null $ name n) && BC.head (name n) == '('
          then (BC.takeWhile (/= ')') (BC.drop 2 n) , FunPtr t'')
          else (name n, t'')

lengths :: ByteString -> [Int]
lengths s =
  let s' = BC.drop 1 (BC.dropWhile (/= '[') s)
  in case BC.readInt s' of
       Just (i, s'') -> i : lengths s''
       Nothing -> []

flatten :: [SDNAStruct] -> [Struct]
flatten sdnaStructs = let ss = map (flattenStruct ss) sdnaStructs in ss

----------------------------------------------------------------------
-- Low level blend-file: the block data are left as ByteStrings
----------------------------------------------------------------------

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
    "DNA1" -> do skipAndCheck "ENDB"
                 let sdna = runGet (getSDNA $ blendHeader bf) $ LB.fromChunks [blckData b]
                 return $ bf { blendSdna = sdna }
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
----------------------------------------------------------------------
-- Parsing primitives.
-- The blend-file header is given in argument to account for
-- the endiannes and the pointer-size used to write the file.
-- The size of a function pointer is assumed to be the size of
-- a regular pointer.
----------------------------------------------------------------------

getWord16 :: BHeader -> Get Word16
getWord16 h = case endianness h of
  LittleEndian -> getWord16le
  BigEndian -> getWord16be

getWord32 :: BHeader -> Get Word32
getWord32 h = case endianness h of
  LittleEndian -> getWord32le
  BigEndian -> getWord32be

getWord64 :: BHeader -> Get Word64
getWord64 h = case endianness h of
  LittleEndian -> getWord64le
  BigEndian -> getWord64be

-- Returns an address according to the endianness an
-- pointer size of a given header.
getAddress :: BHeader -> Get Integer
getAddress h = case pointerSize h of
  Pointer32 -> fromIntegral <$> getWord32 h
  Pointer64 -> fromIntegral <$> getWord64 h

getPointer :: BHeader -> Get Integer
getPointer = getAddress

getShort :: BHeader -> Get Int16
getShort h = fromIntegral <$> getWord16 h

getUShort :: BHeader -> Get Word16
getUShort h = fromIntegral <$> getWord16 h

getInt :: BHeader -> Get Int32
getInt h = fromIntegral <$> getWord32 h

getLong :: BHeader -> Get Int64
getLong h = fromIntegral <$> getWord64 h

getULong :: BHeader -> Get Word64
getULong h = fromIntegral <$> getWord64 h

-- Thanks to Judah Jacobson on the Haskell mailing-list for the
-- Word32/Float and Word64/Double conversions.

getFloat :: BHeader -> Get Float
getFloat h = coerce <$> getWord32 h
 where
   coerce w = unsafePerformIO $ with w $ \p -> do
                d <- peek (castPtr p) :: IO CFloat
                return (realToFrac d :: Float)

getDouble :: BHeader -> Get Double
getDouble h = coerce <$> getWord64 h
 where
   coerce w = unsafePerformIO $ with w $ \p -> do
                d <- peek (castPtr p) :: IO CDouble
                return (realToFrac d :: Double)

skipAddress :: BHeader -> Get ()
skipAddress h = case pointerSize h of
  Pointer32 -> skip 4
  Pointer64 -> skip 8

skipAndCheck :: ByteString -> Get ()
skipAndCheck s = do
  b <- getByteString (BC.length s)
  when (b /= s) $ error $ "Expected " ++ BC.unpack s ++
    ", read " ++ BC.unpack b ++ ".\n"

-- Parses a null-terminated string (doesn't return the null byte).
getNulTerminatedString :: Get ByteString
getNulTerminatedString = pack <$> go
  where go :: Get [Word8]
        go = do w <- get
                if w == 0
                  then return []
                  else do ws <- go
                          return (w : ws)

-- Parses n null-terminated strings.
getNulTerminatedStrings :: Integral a => a -> Get [ByteString]
getNulTerminatedStrings n =
  replicateM (fromIntegral n) getNulTerminatedString

-- Skips bytes if necessary to ensure proper parsing of aligned data.
align :: Int64 -> Get ()
align a = do
  n <- bytesRead
  case n `mod` a of
    0 -> return ()
    r -> skip (fromIntegral $ a - r)

