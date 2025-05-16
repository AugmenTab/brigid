module Generation.Generators
  ( text
  , nonEmptyText
  ) where

import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

char :: Gen Char
char =
  Gen.enumBounded

text :: Gen T.Text
text = Gen.text (Range.linear 1 100) Gen.unicode

nonEmptyText :: Gen NET.NonEmptyText
nonEmptyText =
  NET.new
    <$> char
    <*> text
