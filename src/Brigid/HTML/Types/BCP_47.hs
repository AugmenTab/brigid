module Brigid.HTML.Types.BCP_47
  ( BCP_47
  , bcp47ToBytes
  , bcp47ToText
  , bcp47Language
  ) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.LanguageCodes (ISO639_1, language, toChars)
import Data.Text qualified as T

-- TODO: This is a minimal definition. Proper representation of BCP-47 language
-- codes will require A LOT more work.
--
type BCP_47 = ISO639_1

bcp47ToBytes :: BCP_47 -> LBS8.ByteString
bcp47ToBytes =
  LBS8.pack . show . (\(c1, c2) -> [ c1, c2 ]) . toChars

bcp47ToText :: BCP_47 -> T.Text
bcp47ToText =
  T.pack . show . (\(c1, c2) -> [ c1, c2 ]) . toChars

bcp47Language :: BCP_47 -> T.Text
bcp47Language = T.pack . language

-- TODO: The language's linguonym (as the native speakers refer to it) as Text
--
-- bcp47Linguonym

-- TODO: The language's linguonym (as the native speakers refer to it),
-- romanized, as Text
--
-- bcp47LinguonymRomanized
