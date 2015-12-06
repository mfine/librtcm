-- |
-- Module:      Data.RTCM3.Observations
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- RTCMv3 Observations.

module Data.RTCM3.Observations where

import           BasicPrelude
import           Control.Lens
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import           Data.Int
import           Data.RTCM3.Extras
import           Data.RTCM3.TH

data GpsObservationHeader = GpsObservationHeader
  { _gpsObservationsHeader_num                :: Word16
  , _gpsObservationsHeader_station            :: Word16
  , _gpsObservationsHeader_tow                :: Word32
  , _gpsObservationsHeader_synchronous        :: Bool
  , _gpsObservationsHeader_n                  :: Word8
  , _gpsObservationsHeader_smoothingIndicator :: Bool
  , _gpsObservationsHeader_smoothingInterval  :: Word8
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsObservationHeader)

instance BinaryBit GpsObservationHeader where
  getBits _n = do
    _gpsObservationsHeader_num                <- B.getWord16be 12
    _gpsObservationsHeader_station            <- B.getWord16be 12
    _gpsObservationsHeader_tow                <- B.getWord32be 30
    _gpsObservationsHeader_synchronous        <- B.getBool
    _gpsObservationsHeader_n                  <- B.getWord8 5
    _gpsObservationsHeader_smoothingIndicator <- B.getBool
    _gpsObservationsHeader_smoothingInterval  <- B.getWord8 3
    return GpsObservationHeader {..}

  putBits _n GpsObservationHeader {..} = do
    B.putWord16be 12 _gpsObservationsHeader_num
    B.putWord16be 12 _gpsObservationsHeader_station
    B.putWord32be 30 _gpsObservationsHeader_tow
    B.putWord32be 30 _gpsObservationsHeader_tow
    B.putBool        _gpsObservationsHeader_synchronous
    B.putWord8 5     _gpsObservationsHeader_n
    B.putBool        _gpsObservationsHeader_smoothingIndicator
    B.putWord8 3     _gpsObservationsHeader_smoothingInterval

data GpsL1Observation = GpsL1Observation
  { _gpsL1Observation_codeIndicator     :: Bool
  , _gpsL1Observation_pseudorange       :: Word32
  , _gpsL1Observation_carrierMinusCode  :: Int32
  , _gpsL1Observation_lockTimeIndicator :: Word8
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL1Observation)

instance BinaryBit GpsL1Observation where
  getBits _n = do
    _gpsL1Observation_codeIndicator     <- B.getBool
    _gpsL1Observation_pseudorange       <- B.getWord32be 24
    _gpsL1Observation_carrierMinusCode  <- getInt32be 20
    _gpsL1Observation_lockTimeIndicator <- B.getWord8 7
    return GpsL1Observation {..}

  putBits _n GpsL1Observation {..} = do
    B.putBool        _gpsL1Observation_codeIndicator
    B.putWord32be 24 _gpsL1Observation_pseudorange
--  B.putInt32be 20  _gpsL1Observation_carrierMinusCode  TODO
    B.putWord8 7     _gpsL1Observation_lockTimeIndicator

data GpsL1ExtObservation = GpsL1ExtObservation
  { _gpsL1ExtObservation_ambiguity :: Word8
  , _gpsL1ExtObservation_cnr       :: Word8
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL1ExtObservation)

instance BinaryBit GpsL1ExtObservation where
  getBits _n = do
    _gpsL1ExtObservation_ambiguity <- B.getWord8 8
    _gpsL1ExtObservation_cnr       <- B.getWord8 8
    return GpsL1ExtObservation {..}

  putBits _n GpsL1ExtObservation {..} = do
    B.putWord8 8 _gpsL1ExtObservation_ambiguity
    B.putWord8 8 _gpsL1ExtObservation_cnr


data GpsL2Observation = GpsL2Observation
  { _gpsL2Observation_codeIndicator         :: Word8
  , _gpsL2Observation_pseudorangeDifference :: Int16
  , _gpsL2Observation_carrierMinusCode      :: Int32
  , _gpsL2Observation_lockTimeIndicator     :: Word8
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL2Observation)

instance BinaryBit GpsL2Observation where
  getBits _n = do
    _gpsL2Observation_codeIndicator         <- B.getWord8 2
    _gpsL2Observation_pseudorangeDifference <- getInt16be 14
    _gpsL2Observation_carrierMinusCode      <- getInt32be 20
    _gpsL2Observation_lockTimeIndicator     <- B.getWord8 7
    return GpsL2Observation {..}

  putBits _n GpsL2Observation {..} = do
    B.putWord8 2     _gpsL2Observation_codeIndicator
--  B.putInt16be 14  _gpsL2Observation_pseudorangeDifference  TODO
--  B.putInt32be 20  _gpsL2Observation_carrierMinusCode       TODO
    B.putWord8 7     _gpsL2Observation_lockTimeIndicator

data GpsL2ExtObservation = GpsL2ExtObservation
  { _gpsL2ExtObservation_cnr :: Word8
  } deriving ( Show, Read, Eq )

$(makeLenses ''GpsL2ExtObservation)

instance BinaryBit GpsL2ExtObservation where
  getBits _n = do
    _gpsL2ExtObservation_cnr <- B.getWord8 8
    return GpsL2ExtObservation {..}

  putBits _n GpsL2ExtObservation {..} = do
    B.putWord8 8 _gpsL2ExtObservation_cnr

msg1001 :: Word16
msg1001 = 1001

data Observation1001 = Observation1001
  { _observation1001_sat :: Word8
  , _observation1001_l1  :: GpsL1Observation
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1001)

instance BinaryBit Observation1001 where
  getBits n = do
    _observation1001_sat <- B.getWord8 6
    _observation1001_l1  <- getBits n
    return Observation1001 {..}

  putBits n Observation1001 {..} = do
    B.putWord8 6 _observation1001_sat
    putBits n _observation1001_l1

data Msg1001 = Msg1001
  { _msg1001_header       :: GpsObservationHeader
  , _msg1001_observations :: [Observation1001]
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1001)

instance Binary Msg1001 where
  get = B.runBitGet $ do
    _msg1001_header <- getBits 0
    _msg1001_observations <- replicateM (fromIntegral $ _msg1001_header ^. gpsObservationsHeader_n) $ getBits 0
    return Msg1001 {..}

  put Msg1001 {..} = B.runBitPut $ do
    putBits 0 _msg1001_header
    forM_ _msg1001_observations $ putBits 0

$(deriveRTCM3 ''Msg1001)

msg1002 :: Word16
msg1002 = 1002

data Observation1002 = Observation1002
  { _observation1002_sat :: Word8
  , _observation1002_l1  :: GpsL1Observation
  , _observation1002_l1e :: GpsL1ExtObservation
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1002)

instance BinaryBit Observation1002 where
  getBits n = do
    _observation1002_sat <- B.getWord8 6
    _observation1002_l1  <- getBits n
    _observation1002_l1e <- getBits n
    return Observation1002 {..}

  putBits n Observation1002 {..} = do
    B.putWord8 6 _observation1002_sat
    putBits n _observation1002_l1
    putBits n _observation1002_l1e

data Msg1002 = Msg1002
  { _msg1002_header       :: GpsObservationHeader
  , _msg1002_observations :: [Observation1002]
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1002)

instance Binary Msg1002 where
  get = B.runBitGet $ do
    _msg1002_header <- getBits 0
    _msg1002_observations <- replicateM (fromIntegral $ _msg1002_header ^. gpsObservationsHeader_n) $ getBits 0
    return Msg1002 {..}

  put Msg1002 {..} = B.runBitPut $ do
    putBits 0 _msg1002_header
    forM_ _msg1002_observations $ putBits 0

$(deriveRTCM3 ''Msg1002)

msg1003 :: Word16
msg1003 = 1003

data Observation1003 = Observation1003
  { _observation1003_sat :: Word8
  , _observation1003_l1  :: GpsL1Observation
  , _observation1003_l1e :: GpsL1ExtObservation
  , _observation1003_l2  :: GpsL2Observation
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1003)

instance BinaryBit Observation1003 where
  getBits n = do
    _observation1003_sat <- B.getWord8 6
    _observation1003_l1  <- getBits n
    _observation1003_l1e <- getBits n
    _observation1003_l2  <- getBits n
    return Observation1003 {..}

  putBits n Observation1003 {..} = do
    B.putWord8 6 _observation1003_sat
    putBits n _observation1003_l1
    putBits n _observation1003_l1e
    putBits n _observation1003_l2

data Msg1003 = Msg1003
  { _msg1003_header       :: GpsObservationHeader
  , _msg1003_observations :: [Observation1003]
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1003)

instance Binary Msg1003 where
  get = B.runBitGet $ do
    _msg1003_header <- getBits 0
    _msg1003_observations <- replicateM (fromIntegral $ _msg1003_header ^. gpsObservationsHeader_n) $ getBits 0
    return Msg1003 {..}

  put Msg1003 {..} = B.runBitPut $ do
    putBits 0 _msg1003_header
    forM_ _msg1003_observations $ putBits 0

$(deriveRTCM3 ''Msg1003)

msg1004 :: Word16
msg1004 = 1004

data Observation1004 = Observation1004
  { _observation1004_sat :: Word8
  , _observation1004_l1  :: GpsL1Observation
  , _observation1004_l1e :: GpsL1ExtObservation
  , _observation1004_l2  :: GpsL2Observation
  , _observation1004_l2e :: GpsL2ExtObservation
  } deriving ( Show, Read, Eq )

$(makeLenses ''Observation1004)

instance BinaryBit Observation1004 where
  getBits n = do
    _observation1004_sat <- B.getWord8 6
    _observation1004_l1  <- getBits n
    _observation1004_l1e <- getBits n
    _observation1004_l2  <- getBits n
    _observation1004_l2e <- getBits n
    return Observation1004 {..}

  putBits n Observation1004 {..} = do
    B.putWord8 6 _observation1004_sat
    putBits n _observation1004_l1
    putBits n _observation1004_l1e
    putBits n _observation1004_l2
    putBits n _observation1004_l2e

data Msg1004 = Msg1004
  { _msg1004_header       :: GpsObservationHeader
  , _msg1004_observations :: [Observation1004]
  } deriving ( Show, Read, Eq )

$(makeLenses ''Msg1004)

instance Binary Msg1004 where
  get = B.runBitGet $ do
    _msg1004_header <- getBits 0
    _msg1004_observations <- replicateM (fromIntegral $ _msg1004_header ^. gpsObservationsHeader_n) $ getBits 0
    return Msg1004 {..}

  put Msg1004 {..} = B.runBitPut $ do
    putBits 0 _msg1004_header
    forM_ _msg1004_observations $ putBits 0

$(deriveRTCM3 ''Msg1004)
