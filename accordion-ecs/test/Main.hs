{-# language QuasiQuotes #-}

import Data.ByteString.Short (ShortByteString,toShort)
import Data.Primitive (ByteArray)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (text)
import Control.Monad (when)
import Accordion.Types (Rec(RecCons,RecNil))
import Accordion.Ecs.Zeek (zeekToEcs)
import Data.Char (chr)

import qualified Accordion.Ecs as Ecs
import qualified GHC.Exts as Exts
import qualified Data.Json.Tokenize as J
import qualified Zeek.Json as Zeek
import qualified Data.Primitive as PM
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes as Bytes
import qualified Data.Arithmetic.Nat as Nat
import qualified World.Word64 as Word64
import qualified Data.Foldable as F

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  putStrLn "Finished"

testA :: IO ()
testA = case Zeek.decode (Bytes.fromByteArray encodedDnsLog) of
  Left _ -> fail "testA: could not parse JSON as zeek"
  Right zattrs -> case zeekToEcs (pure zattrs) of
    Nothing -> fail "testA: timestamp was missing somewhere"
    Just attrs -> do
      let expected = Ecs.Attributes
            Nat.one
            ( Word64.singleton 1565528573524392000)
            ( Ecs.single2 Ecs.dns Ecs.question Ecs.classNumber 1
              `Ecs.union`
              Ecs.single2 Ecs.dns Ecs.question Ecs.typeNumber 1
              `Ecs.union`
              Ecs.single1 Ecs.source Ecs.port 33550
            )
      let success = expected == attrs
      when (not success) $
        fail $ "testA\nExpected:\n"
          ++ unpack (F.fold (Ecs.toJson expected))
          ++ "\nbut got\n"
          ++ unpack (F.fold (Ecs.toJson attrs))
                 

unpack :: ByteArray -> String
unpack = map (chr . fromIntegral) . Exts.toList

encodedDnsLog :: ByteArray
encodedDnsLog = shortByteStringToByteArray $ toShort $ encodeUtf8
  [text|
    {
      "@path": "dns",
      "@sensor": "corelight",
      "@timestamp": "2019-08-11T13:02:53.529992Z",
      "AA": true,
      "RA": true,
      "RD": true,
      "TC": false,
      "TTLs": [ 3600,7200,300, 1200 ],
      "Z": 0,
      "answers": ["192.0.2.231" ,"192.0.2.232","192.0.2.233", "192.0.2.234"],
      "id.orig_h": "192.0.2.11",
      "id.orig_p": 33550,
      "id.resp_h": "192.0.2.25",
      "id.resp_p": 53,
      "proto": "udp",
      "qclass": 1,
      "qclass_name": "C_INTERNET",
      "qtype": 1,
      "qtype_name": "A",
      "query": "foo.example.com",
      "rcode": 0,
      "rcode_name": "NOERROR",
      "rejected": false,
      "rtt": 0.0056,
      "trans_id": 53269,
      "ts": "2019-08-11T13:02:53.524392Z",
      "uid": "Cv2WQgF86tV38UAS8"
    }
  |]
