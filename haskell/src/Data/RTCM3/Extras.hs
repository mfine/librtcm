-- |
-- Module:      Data.RTCM3.Extras
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Extra stuff.

module Data.RTCM3.Extras
  ( getInt8
  , getInt16be
  , getInt32be
  , getWord24be
  , putWord24be
  ) where

import           BasicPrelude
import           Data.Binary
import qualified Data.Binary.Bits.Get as B
import           Data.Bits
import           Data.Int
import           Data.Word.Word24

signExtend8 :: Int -> Word8 -> Int8
signExtend8 bits x = (fromIntegral x `shiftL` (8 - bits)) `shiftR` (8 - bits)

signExtend16 :: Int -> Word16 -> Int16
signExtend16 bits x = (fromIntegral x `shiftL` (16 - bits)) `shiftR` (16 - bits)

signExtend32 :: Int -> Word32 -> Int32
signExtend32 bits x = (fromIntegral x `shiftL` (32 - bits)) `shiftR` (32 - bits)

getInt8 :: Int -> B.BitGet Int8
getInt8 bits = signExtend8 bits <$> B.getWord8 bits

getInt16be :: Int -> B.BitGet Int16
getInt16be bits = signExtend16 bits <$> B.getWord16be bits

getInt32be :: Int -> B.BitGet Int32
getInt32be bits = signExtend32 bits <$> B.getWord32be bits

getWord24be :: Get Word24
getWord24be = do
    b1 <- fromIntegral <$> getWord8
    b2 <- fromIntegral <$> getWord8
    b3 <- fromIntegral <$> getWord8
    return $ fromInteger $ shiftL b1 16 .|. shiftL b2 8 .|. b3
{-# INLINE getWord24be #-}

putWord24be :: Word24 -> Put
putWord24be w = do
    putWord8 $ fromIntegral $ shiftR w 16
    putWord8 $ fromIntegral $ shiftR w 8
    putWord8 $ fromIntegral w
{-# INLINE putWord24be #-}
