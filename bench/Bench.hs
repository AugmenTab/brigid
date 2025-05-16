module Main
  ( main
  ) where

import Test.Tasty.Bench qualified as TB
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (treeValue)
import Hedgehog.Range (Size (..))

import Generate qualified
import Generation.Analysis (totalNodes)

main :: IO ()
main = do
  let
    size = Size 30
    seed = from 1000

  case evalGen size seed (Generate.html 8 10) of
    Nothing ->
      error "Failed to generate DOM tree."

    Just tree -> do
      let
        html = treeValue tree

      print html
      print $ totalNodes html
      TB.defaultMain
        [
        ]

  {-
Html []
  [ Head []
      [ Style []
      ]
  , Body []
      [ Subscript []
          [ Underline []
              [ IdiomaticText []
                  [ BidirectionalIsolation []
                      [ BidirectionalOverride []
                          [ IFrame []
                          ,Comment "\707692\278580\70396\702237\284441\100605\102835\373112\165314\634611\241726\930274\292191\751529\825283\397558\532078\403867\715516\290325\751487\873616\735431\632589\243627\840540\613551",Script [] "\632034\790970\2137\804941\30526\397366\886289\534464\903157\393007\209931\145758\899481\781287\730627\573502\609498\374980\130474\946249\400888\207588\814587\605859\372308\82829\189058\153649\5666"
                          ]
                      ]
                  , Picture [] []
                  , Subscript []
                      [ Slot []
                          [ Text "\235161\390788\826150\24874\415888\976385\831910\201622\648419\10672\37774\770916\449345\578139\686012\430345\832644\294665\509825"
                          ]
                      , Data []
                          [ Code []
                              [ Video []
                                  [ Track []
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      , Emphasis [] []
      , Progress [] []
      ]
  ]
  -}
