-- |
-- Module:      Data.RTCM3.Types
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Common RTCMv3 type requirements, containers, and serialization
-- utilities.

module Data.RTCM3.Types where

import           BasicPrelude
import           Control.Lens
import           Data.Binary
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Builder
import           Data.CRC24Q
import           Data.RTCM3.Extras
import           Data.Word.Word24

frameRTCM3Preamble :: Word8
frameRTCM3Preamble = 0xD3

data Frame = Frame
  { _frameRTCM3Len     :: Word16
  , _frameRTCM3Payload :: !ByteString
  , _frameRTCM3Crc     :: Word24
  } deriving ( Show, Read, Eq )

$(makeLenses ''Frame)

instance Binary Frame where
  get = do
    _frameRTCM3Len <- B.runBitGet $ do
      void $ B.getWord16be 6
      B.getWord16be 10
    _frameRTCM3Payload <- getByteString $ fromIntegral _frameRTCM3Len
    _frameRTCM3Crc     <- getWord24be
    return Frame {..}

  put Frame {..} = do
    B.runBitPut $ do
      B.putWord16be 6 0
      B.putWord16be 10 _frameRTCM3Len
    putByteString _frameRTCM3Payload
    putWord24be _frameRTCM3Crc

checkCrc :: Frame -> Word24
checkCrc Frame {..} =
  crc24q $ toLazyByteString $
    word16BE _frameRTCM3Len <>
    byteString _frameRTCM3Payload

