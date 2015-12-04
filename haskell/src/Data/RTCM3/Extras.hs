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
  ( getWord24be
  , putWord24be
  ) where

import BasicPrelude
import Data.Binary
import Data.Bits
import Data.Word.Word24

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
