-- |
-- Module:      Data.RTCM3.Antennas
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 Antennas.

module Data.RTCM3.Antennas where

import           BasicPrelude
import           Control.Lens
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Int
import           Data.RTCM3.Extras
import           Data.RTCM3.TH

data AntennaReference = AntennaReference
  { _antennaReference_num        :: Word16
  , _antennaReference_station    :: Word16
  , _antennaReference_gps        :: Bool
  , _antennaReference_glonass    :: Bool
  , _antennaReference_galileo    :: Bool
  , _antennaReference_computed   :: Bool
  , _antennaReference_ecef_x     :: Int64
  , _antennaReference_oscillator :: Bool
  , _antennaReference_ecef_y     :: Int64
  , _antennaReference_ecef_z     :: Int64
  } deriving ( Show, Read, Eq )

$(makeLenses ''AntennaReference)

instance BinaryBit AntennaReference where
  getBits _n = do
    _antennaReference_num        <- B.getWord16be 12
    _antennaReference_station    <- B.getWord16be 12
    _antennaReference_gps        <- B.getBool
    _antennaReference_glonass    <- B.getBool
    _antennaReference_galileo    <- B.getBool
    _antennaReference_computed   <- B.getBool
--  _antennaReference_ecef_x     <- B.getInt64be 38   TODO
    _antennaReference_oscillator <- B.getBool
    _reserved                    <- B.getBool
--  _antennaReference_ecef_y     <- B.getInt64be 38   TODO
    _reserved                    <- B.getWord8 2
--  _antennaReference_ecef_z     <- B.getInt64be 38   TODO
    return AntennaReference {..}

  putBits _n AntennaReference {..} = do
    B.putWord16be 12 _antennaReference_num
    B.putWord16be 12 _antennaReference_station
    B.putBool        _antennaReference_gps
    B.putBool        _antennaReference_glonass
    B.putBool        _antennaReference_galileo
    B.putBool        _antennaReference_computed
--  B.putInt64be 38  _antennaReference_ecef_x       TODO
    B.putBool        False
--  B.putInt64be 38  _antennaReference_ecef_y       TODO
    B.putWord8 2     0
--  B.putInt64be 38  _antennaReference_ecef_z       TODO

data ExtAntennaReference = ExtAntennaReference
  { _extAntennaReference_height :: Word16
  } deriving ( Show, Read, Eq )

$(makeLenses ''ExtAntennaReference)

instance BinaryBit ExtAntennaReference where
  getBits _n = do
    _extAntennaReference_height <- B.getWord16be 16
    return ExtAntennaReference {..}

  putBits _n ExtAntennaReference {..} = do
    B.putWord16be 16 _extAntennaReference_height

msg1005 :: Word16
msg1005 = 1005

data Msg1005 = Msg1005
  { _msg1005_reference :: AntennaReference
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1005)

instance Binary Msg1005 where
  get = B.runBitGet $ do
    _msg1005_reference <- getBits 0
    return Msg1005 {..}

  put Msg1005 {..} = B.runBitPut $ do
    putBits 0 _msg1005_reference

$(deriveRTCM3 ''Msg1005)

msg1006 :: Word16
msg1006 = 1006

data Msg1006 = Msg1006
  { _msg1006_reference    :: AntennaReference
  , _msg1006_extReference :: ExtAntennaReference
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1006)

instance Binary Msg1006 where
  get = B.runBitGet $ do
    _msg1006_reference    <- getBits 0
    _msg1006_extReference <- getBits 0
    return Msg1006 {..}

  put Msg1006 {..} = B.runBitPut $ do
    putBits 0 _msg1006_reference
    putBits 0 _msg1006_extReference

$(deriveRTCM3 ''Msg1006)


