module Builder.Ip
  ( builder
  ) where

import Net.Types (IP(IP),IPv6(IPv6))
import Data.WideWord (Word128)

import qualified Arithmetic.Nat as Nat
import qualified Data.ByteArray.Builder as B
import qualified Net.IP as IP

builder :: Word128 -> B.Builder 
builder =
    B.fromBounded Nat.constant
  . IP.boundedBuilderUtf8
  . IP
  . IPv6

