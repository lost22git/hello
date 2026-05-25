-- String = [char]
-- Text = UTF-16 vector
-- ByteString = word8 vector
-- String <-> Text <-> ByteString

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

stringToText = T.pack

textToString = T.unpack

byteStringToText = TE.decodeUtf8

textToByteString = TE.encodeUtf8

-- Thanks to pragma LANGUAGE OverloadedStrings,
-- we can define a Text from a String literal,
-- instead of Text.pack String
msg :: T.Text
msg = "hello from love"
