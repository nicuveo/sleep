module Web.Sleep.Tumblr.Parameters where

import "sleep-core" Prelude

import Data.ByteString      (ByteString)
import Data.Text.Encoding


--------------------------------------------------------------------------------
-- Parameter class

class Parameter a where
  toByteString :: a -> ByteString
  toText       :: a -> Text

instance Parameter ByteString where
  toByteString = id
  toText       = decodeUtf8Lenient

instance Parameter Text where
  toByteString = encodeUtf8
  toText       = id


--------------------------------------------------------------------------------
-- Parameters

newtype BlogId = BlogId ByteString
  deriving Parameter

newtype UserAgent = UserAgent ByteString
  deriving Parameter


data AvatarSize
  = Size16
  | Size24
  | Size30
  | Size40
  | Size48
  | Size64
  | Size96
  | Size128
  | Size512

_renderAS :: IsString s => AvatarSize -> s
_renderAS = \case
  Size16  ->  "16"
  Size24  ->  "24"
  Size30  ->  "30"
  Size40  ->  "40"
  Size48  ->  "48"
  Size64  ->  "64"
  Size96  ->  "96"
  Size128 -> "128"
  Size512 -> "512"

instance Parameter AvatarSize where
  toByteString = _renderAS
  toText       = _renderAS
