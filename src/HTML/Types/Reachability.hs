module HTML.Types.Reachability
  ( Reachability
      ( Reachable
      , NotReachable
      )
  , reachabilityToInt
  ) where

data Reachability
  = Reachable
  | NotReachable

reachabilityToInt :: Reachability -> Int
reachabilityToInt option =
  case option of
    Reachable    -> 0
    NotReachable -> negate 1
