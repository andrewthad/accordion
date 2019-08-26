{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language GADTSyntax #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Accordion.Ecs.Types
  ( FieldHeight
  , PrefixHeight
  , ManyHeight
  , Field(..)
  , Prefix(..)
  , Universe
  , Index
  , IndexPrefix
  , SingUniverse
  , SingField(..)
  , SingPrefix(..)
  , Ground
  , GroundWorld
  , Unindex
  , Interpret
  , Represent
  , singFieldHeight
  , index
  , indexPrefix
  , interpret
  , represent
  , toInternal
  , fromInternal
  , encodeField
  , encodePrefix
  , pasteMany
  , pasteManyOpt
  ) where

import Accordion.World (World(..),GroundWorld)
import Accordion.Types (Nat(..),Vec(..),Omnitree(..),Finger)
import Accordion.Nat (N2,N1)
import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))
import Data.Text.Short (ShortText)
import Accordion.Types (Nat(..),Vec(..),Omnitree(..),SingNat,SingBool(..))
import Accordion.Types (Finger(..))
import Accordion.Nat (N6,n6,N1)
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Accordion.World (World,SingWorld)
import Chronos (Time)
import Net.Types (IPv4,Mac)
import Data.Int (Int64)
import Data.Word (Word16,Word64)

import qualified Data.Tuple.Types as Tuple
import qualified Accordion.Binary.N6 as Bin
import qualified Accordion.World as W
import qualified Accordion.Json.Types as Json
import qualified Accordion.Json.Encode as Encode
import qualified Accordion.Types as A
import qualified Accordion.World as A

type FieldHeight = N6
type PrefixHeight = N6
type ManyHeight = N1

-- showsPrecUniverse :: SingUniverse u -> Int -> Ground u -> ShowS
-- showsPrecUniverse SingNumber = showsPrec
-- showsPrecUniverse SingCharacter = showsPrec
-- showsPrecUniverse SingBoolean = showsPrec

toInternal ::
     SingUniverse u
  -> Ground u
  -> GroundWorld (Represent u)
toInternal n x = case n of
  SingUniWord64 -> x

fromInternal ::
     SingUniverse u
  -> GroundWorld (Represent u)
  -> Ground u
fromInternal n x = case n of
  SingUniWord64 -> x

encodeField :: Finger @FieldHeight v -> ShortText
encodeField x = case unindexField x of
  SingAddress -> "address"
  SingBytes -> "bytes"
  SingPort -> "port"

encodePrefix :: Finger @PrefixHeight v -> ShortText
encodePrefix x = case unindexPrefix x of
  SingSource -> "source"
  SingDestination -> "destination"

pasteMany ::
     Finger @FieldHeight v
  -> Json.Encode (A.VectorizeWorld (Represent (Interpret v)))
pasteMany x = case unindexField x of
  SingPort -> Encode.word16

pasteManyOpt ::
     Finger @FieldHeight v
  -> Json.EncodeOptional (A.VectorizeWorld (Represent (Interpret v)))
pasteManyOpt x = case unindexField x of
  SingPort -> Encode.word16Opt

-- Omitted fields: labels
data Field
  = Address -- keyword
  | Bytes -- long
  | CityName -- keyword
  | Class -- keyword
  | ClassNumber -- keyword
  | Content -- keyword
  | CountryIsoCode -- keyword
  | CountryName -- keyword
  | Domain -- keyword
  | Email -- keyword
  | EphemeralId -- keyword
  | Id -- keyword
  | Ip -- ip address
  | Location -- geo_point
  | Mac -- keyword (MAC address)
  | Message -- text
  | Method -- keyword
  | Name -- keyword
  | Packets -- long
  | Port -- long (16-bit word)
  | Referrer -- keyword
  | ResponseCode -- keyword
  | StatusCode -- long
  | Tags -- list of keyword
  | Timestamp -- date
  | Type -- keyword
  | Version -- keyword

data Prefix
  = Agent
  | AutonomousSystem
  | Body
  | Client
  | Cloud
  | Container
  | Destination
  | Dns
  | Ecs
  | Error
  | Event
  | File
  | Geo
  | Group
  | Host
  | Http
  | Log
  | Network
  | Observer
  | OperatingSystem
  | Organization
  | Process
  | Question
  | Related
  | Request
  | Server
  | Service
  | Source
  | Url
  | User
  | UserAgent

data SingField :: Field -> Type where
  SingAddress :: SingField 'Address
  SingBytes :: SingField 'Bytes
  SingCityName :: SingField 'CityName
  SingClass :: SingField 'Class
  SingClassNumber :: SingField 'ClassNumber
  SingContent :: SingField 'Content
  SingCountryIsoCode :: SingField 'CountryIsoCode
  SingCountryName :: SingField 'CountryName
  SingDomain :: SingField 'Domain
  SingEmail :: SingField 'Email
  SingEphemeralId :: SingField 'EphemeralId
  SingId :: SingField 'Id
  SingIp :: SingField 'Ip
  SingLocation :: SingField 'Location
  SingMac :: SingField 'Mac
  SingMessage :: SingField 'Message
  SingMethod :: SingField 'Method
  SingName :: SingField 'Name
  SingPackets :: SingField 'Packets
  SingPort :: SingField 'Port
  SingReferrer :: SingField 'Referrer
  SingResponseCode :: SingField 'ResponseCode
  SingStatusCode :: SingField 'StatusCode
  SingTags :: SingField 'Tags
  SingTimestamp :: SingField 'Timestamp
  SingType :: SingField 'Type
  SingVersion :: SingField 'Version

data SingPrefix :: Prefix -> Type where
  SingAgent :: SingPrefix 'Agent
  SingAutonomousSystem :: SingPrefix 'AutonomousSystem
  SingBody :: SingPrefix 'Body
  SingClient :: SingPrefix 'Client
  SingCloud :: SingPrefix 'Cloud
  SingContainer :: SingPrefix 'Container
  SingDestination :: SingPrefix 'Destination
  SingDns :: SingPrefix 'Dns
  SingEcs :: SingPrefix 'Ecs
  SingError :: SingPrefix 'Error
  SingEvent :: SingPrefix 'Event
  SingFile :: SingPrefix 'File
  SingGeo :: SingPrefix 'Geo
  SingGroup :: SingPrefix 'Group
  SingHost :: SingPrefix 'Host
  SingHttp :: SingPrefix 'Http
  SingLog :: SingPrefix 'Log
  SingNetwork :: SingPrefix 'Network
  SingObserver :: SingPrefix 'Observer
  SingOperatingSystem :: SingPrefix 'OperatingSystem
  SingOrganization :: SingPrefix 'Organization
  SingProcess :: SingPrefix 'Process
  SingQuestion :: SingPrefix 'Question
  SingRelated :: SingPrefix 'Related
  SingRequest :: SingPrefix 'Request
  SingServer :: SingPrefix 'Server
  SingService :: SingPrefix 'Service
  SingSource :: SingPrefix 'Source
  SingUrl :: SingPrefix 'Url
  SingUser :: SingPrefix 'User
  SingUserAgent :: SingPrefix 'UserAgent

data Universe
  = UniGeo
  | UniInt64
  | UniIp
  | UniMac
  | UniTags
  | UniText
  | UniTime
  | UniWord16
  | UniWord32
  | UniWord64

data SingUniverse :: Universe -> Type where
  SingUniGeo :: SingUniverse 'UniGeo
  SingUniInt64 :: SingUniverse 'UniInt64
  SingUniIp :: SingUniverse 'UniIp
  SingUniMac :: SingUniverse 'UniMac
  SingUniTags :: SingUniverse 'UniTags
  SingUniText :: SingUniverse 'UniText
  SingUniTime :: SingUniverse 'UniTime
  SingUniWord16 :: SingUniverse 'UniWord16
  SingUniWord64 :: SingUniverse 'UniWord64

type family Unindex (v :: Vec N6 Bool) :: Field where
  Unindex Bin.N0 = 'Address
  Unindex Bin.N1 = 'Bytes
  Unindex Bin.N2 = 'CityName
  Unindex Bin.N3 = 'Content
  Unindex Bin.N4 = 'CountryIsoCode
  Unindex Bin.N5 = 'CountryName
  Unindex Bin.N6 = 'Domain
  Unindex Bin.N7 = 'Email
  Unindex Bin.N8 = 'EphemeralId
  Unindex Bin.N9 = 'Id
  Unindex Bin.N10 = 'Ip
  Unindex Bin.N11 = 'Location
  Unindex Bin.N12 = 'Mac
  Unindex Bin.N13 = 'Message
  Unindex Bin.N14 = 'Method
  Unindex Bin.N15 = 'Name
  Unindex Bin.N16 = 'Packets
  Unindex Bin.N17 = 'Port
  Unindex Bin.N18 = 'Referrer
  Unindex Bin.N19 = 'StatusCode
  Unindex Bin.N20 = 'Tags
  Unindex Bin.N21 = 'Timestamp
  Unindex Bin.N22 = 'Type
  Unindex Bin.N23 = 'Version
  Unindex Bin.N24 = 'Class
  Unindex Bin.N25 = 'ClassNumber
  Unindex Bin.N26 = 'ResponseCode

type family UnindexPrefix (v :: Vec PrefixHeight Bool) :: Prefix where
  UnindexPrefix Bin.N0 = 'Source
  UnindexPrefix Bin.N1 = 'Destination

type family Index (d :: Field) :: Vec N6 Bool where
  Index 'Address = Bin.N0
  Index 'Bytes = Bin.N1
  Index 'CityName = Bin.N2
  Index 'Content = Bin.N3
  Index 'CountryIsoCode = Bin.N4
  Index 'CountryName = Bin.N5
  Index 'Domain = Bin.N6
  Index 'Email = Bin.N7
  Index 'EphemeralId = Bin.N8
  Index 'Id = Bin.N9
  Index 'Ip = Bin.N10
  Index 'Location = Bin.N11
  Index 'Mac = Bin.N12
  Index 'Message = Bin.N13
  Index 'Method = Bin.N14
  Index 'Name = Bin.N15
  Index 'Packets = Bin.N16
  Index 'Port = Bin.N17
  Index 'Referrer = Bin.N18
  Index 'StatusCode = Bin.N19
  Index 'Tags = Bin.N20
  Index 'Timestamp = Bin.N21
  Index 'Type = Bin.N22
  Index 'Version = Bin.N23
  Index 'Class = Bin.N24
  Index 'ClassNumber = Bin.N25
  Index 'ResponseCode = Bin.N26

type family IndexPrefix (d :: Prefix) :: Vec PrefixHeight Bool where
  IndexPrefix 'Agent = Bin.N0
  IndexPrefix 'AutonomousSystem = Bin.N1
  IndexPrefix 'Body = Bin.N2
  IndexPrefix 'Client = Bin.N3
  IndexPrefix 'Cloud = Bin.N4
  IndexPrefix 'Container = Bin.N5
  IndexPrefix 'Destination = Bin.N6
  IndexPrefix 'Dns = Bin.N7
  IndexPrefix 'Ecs = Bin.N8
  IndexPrefix 'Error = Bin.N9
  IndexPrefix 'Event = Bin.N10
  IndexPrefix 'File = Bin.N11
  IndexPrefix 'Geo = Bin.N12
  IndexPrefix 'Group = Bin.N13
  IndexPrefix 'Host = Bin.N14
  IndexPrefix 'Http = Bin.N15
  IndexPrefix 'Log = Bin.N16
  IndexPrefix 'Network = Bin.N17
  IndexPrefix 'Observer = Bin.N18
  IndexPrefix 'OperatingSystem = Bin.N19
  IndexPrefix 'Organization = Bin.N20
  IndexPrefix 'Process = Bin.N21
  IndexPrefix 'Question = Bin.N22
  IndexPrefix 'Related = Bin.N23
  IndexPrefix 'Request = Bin.N24
  IndexPrefix 'Server = Bin.N25
  IndexPrefix 'Service = Bin.N26
  IndexPrefix 'Source = Bin.N27
  IndexPrefix 'Url = Bin.N28
  IndexPrefix 'User = Bin.N29
  IndexPrefix 'UserAgent = Bin.N30

type family Interpret (v :: Vec N6 Bool) :: Universe where
  Interpret x = InterpretField (Unindex x)

type family InterpretField (d :: Field) :: Universe where
  InterpretField 'Address = 'UniText
  InterpretField 'Bytes = 'UniInt64
  InterpretField 'CityName = 'UniText
  InterpretField 'Content = 'UniText
  InterpretField 'CountryIsoCode = 'UniText
  InterpretField 'CountryName = 'UniText
  InterpretField 'Domain = 'UniText
  InterpretField 'Email = 'UniText
  InterpretField 'EphemeralId = 'UniText
  InterpretField 'Id = 'UniText
  InterpretField 'Ip = 'UniIp
  InterpretField 'Location = 'UniGeo
  InterpretField 'Mac = 'UniMac
  InterpretField 'Message = 'UniText
  InterpretField 'Method = 'UniText
  InterpretField 'Name = 'UniText
  InterpretField 'Packets = 'UniInt64
  InterpretField 'Port = 'UniWord16
  InterpretField 'Referrer = 'UniText
  InterpretField 'StatusCode = 'UniInt64
  InterpretField 'Tags = 'UniTags
  InterpretField 'Timestamp = 'UniTime
  InterpretField 'Type = 'UniText
  InterpretField 'Version = 'UniText
  InterpretField 'Class = 'UniText
  InterpretField 'ClassNumber = 'UniWord16
  InterpretField 'ResponseCode = 'UniText

type family Represent (u :: Universe) :: World where
  Represent 'UniGeo = 'W.DoublePair
  Represent 'UniInt64 = 'W.Int64
  Represent 'UniIp = 'W.Word32
  Represent 'UniMac = 'W.Word64
  Represent 'UniTags = 'W.Texts
  Represent 'UniText = 'W.Text
  Represent 'UniTime = 'W.Word64
  Represent 'UniWord16 = 'W.Word16
  Represent 'UniWord64 = 'W.Word64

represent :: SingUniverse u -> SingWorld (Represent u)
represent SingUniGeo = W.SingDoublePair
represent SingUniInt64 = W.SingInt64
represent SingUniIp = W.SingWord32
represent SingUniMac = W.SingWord64
represent SingUniTags = W.SingTexts
represent SingUniText = W.SingText
represent SingUniTime = W.SingWord64
represent SingUniWord16 = W.SingWord16
represent SingUniWord64 = W.SingWord64

type family Ground (u :: Universe) :: Type where
  Ground 'UniGeo = Tuple.DoublePair
  Ground 'UniInt64 = Int64
  Ground 'UniIp = IPv4
  Ground 'UniMac = Mac
  Ground 'UniTags = [ShortText]
  Ground 'UniText = ShortText
  Ground 'UniTime = Time
  Ground 'UniWord16 = Word16
  Ground 'UniWord64 = Word64

singFieldHeight :: SingNat N6
singFieldHeight = n6

unindexField :: forall (v :: Vec N6 Bool). Finger @N6 v -> SingField (Unindex v)
unindexField = error "write me"
-- unindexField (FingerCons SingTrue (FingerCons SingTrue FingerNil)) = SingAge
-- unindexField (FingerCons SingFalse (FingerCons SingTrue FingerNil)) = SingHealth
-- unindexField (FingerCons SingTrue (FingerCons SingFalse FingerNil)) = SingLetter
-- unindexField (FingerCons SingFalse (FingerCons SingFalse FingerNil)) = SingAlive

unindexPrefix :: Finger @PrefixHeight v -> SingPrefix (UnindexPrefix v)
unindexPrefix _ = error "uoehtnu"
-- unindexPrefix (FingerCons SingTrue FingerNil) = SingSource
-- unindexPrefix (FingerCons SingFalse FingerNil) = SingDestination

indexPrefix :: SingPrefix p -> Finger @PrefixHeight (IndexPrefix p)
indexPrefix = error "uhoetnhu"

index :: SingField d -> Finger @FieldHeight (Index d)
index = error "uhoetnuha"
-- index SingAddress = (FingerCons SingTrue (FingerCons SingTrue FingerNil))
-- index SingBytes = (FingerCons SingFalse (FingerCons SingTrue FingerNil))
-- index SingCityName = (FingerCons SingTrue (FingerCons SingFalse FingerNil))
-- index SingContent = (FingerCons SingFalse (FingerCons SingFalse FingerNil))

interpret :: Finger @N6 v -> SingUniverse (Interpret v)
interpret f = case unindexField f of
  SingAddress -> SingUniText
  SingBytes -> SingUniInt64
  SingCityName -> SingUniText
  SingContent -> SingUniText
  SingCountryIsoCode -> SingUniText
  SingCountryName -> SingUniText
  SingDomain -> SingUniText
  SingEmail -> SingUniText
  SingEphemeralId -> SingUniText
  SingId -> SingUniText
  SingIp -> SingUniIp
  SingLocation -> SingUniGeo
  SingMac -> SingUniMac
  SingMessage -> SingUniText
  SingMethod -> SingUniText
  SingName -> SingUniText
  SingPackets -> SingUniInt64
  SingPort -> SingUniWord16
  SingReferrer -> SingUniText
  SingStatusCode -> SingUniInt64
  SingTags -> SingUniTags
  SingTimestamp -> SingUniTime
  SingType -> SingUniText
  SingVersion -> SingUniText
