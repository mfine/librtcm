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

msgRTCM3Preamble :: Word8
msgRTCM3Preamble = 0xD3

data Msg = Msg
  { _msgRTCM3Len     :: Word16
  , _msgRTCM3Payload :: !ByteString
  , _msgRTCM3Crc     :: Word24
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg)

instance Binary Msg where
  get = do
    _msgRTCM3Len <- B.runBitGet $ do
      _reserved <- B.getWord16be 6
      B.getWord16be 10
    _msgRTCM3Payload <- getByteString $ fromIntegral _msgRTCM3Len
    _msgRTCM3Crc     <- getWord24be
    return Msg {..}

  put Msg {..} = do
    B.runBitPut $ do
      B.putWord16be 6 0
      B.putWord16be 10 _msgRTCM3Len
    putByteString _msgRTCM3Payload
    putWord24be _msgRTCM3Crc

checkCrc :: Msg -> Word24
checkCrc Msg {..} =
  crc24q $ toLazyByteString $
    word16BE _msgRTCM3Len <>
    byteString _msgRTCM3Payload

class Binary a => ToRTCM3 a where
  toRTCM3 :: a -> Msg
