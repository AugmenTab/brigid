module Brigid.HTML.Types.Reachability
  ( Reachability
      ( Reachable
      , NotReachable
      )
  , reachabilityToInteger
  ) where

data Reachability
  = NotReachable
  | Reachable
  deriving (Bounded, Enum, Eq, Ord, Show)

reachabilityToInteger :: Reachability -> Integer
reachabilityToInteger option =
  case option of
    Reachable    -> 0
    NotReachable -> negate 1
