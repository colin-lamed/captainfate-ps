module View.Types where

import Prelude
import Browser.WebStorage (WEB_STORAGE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (StateT, runStateT, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Array (concatMap, fromFoldable, null)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DOM (DOM())
import React (ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, readState, writeState)
import Motor.Story (Action, Oid, Story, Object)
import Motor.Util (currentRoom, toObject, toRoom)


data UiState = UiState { options ∷ Array Option
                       , txts    ∷ Array String
                       , clear   ∷ Boolean
                       }
instance showUiState :: Show UiState where
  show (UiState ui) = "UiState { " <> show ui.options <> "," <> show ui.txts <> "," <> show ui.clear <> " }"


instance semigroupUiState :: Semigroup UiState where
  append (UiState ui1) (UiState ui2) =
    UiState { options : ui1.options <> ui2.options
            , txts    : ui1.txts    <> ui2.txts
            , clear   : ui1.clear   || ui2.clear
            }

instance monoidUiState :: Monoid UiState where
  mempty = UiState { options : [], txts : [], clear : false }

type AppState = { story ∷ Story
                , ui    ∷ { options ∷ Array Option
                          , txt     ∷ Array String
                          , newTxt  ∷ Array String
                          }
                }

type SS a = WriterT UiState (StateT Story (Eff ( console    ∷ CONSOLE
                                               , dom        ∷ DOM
                                               , props      ∷ ReactProps
                                               , refs       ∷ ReactRefs ReadOnly
                                               , state      ∷ ReactState ReadWrite
                                               , webStorage ∷ WEB_STORAGE
                                               ))) a


runSS
  ∷ ∀ props a
  . ReactThis props AppState
  → SS a
  → Eff ( console    ∷ CONSOLE
        , dom        ∷ DOM
        , props      ∷ ReactProps
        , refs       ∷ ReactRefs ReadOnly
        , state      ∷ ReactState ReadWrite
        , webStorage ∷ WEB_STORAGE
        ) a
runSS ctx ssa = do
  state ← readState ctx
  Tuple (Tuple a (UiState ui)) story' ← runStateT (runWriterT ssa) state.story
  void $ writeState ctx state { story = story'
                              , ui    = { options : ui.options
                                        , txt     : if ui.clear then [] else state.ui.txt <> state.ui.newTxt
                                        , newTxt  : ui.txts
                                        }
                              }
  pure a


hoistG
  ∷ ∀ a
  . State Story a
  → SS a
hoistG =
  lift <<< hoist generalize

data Option
  = ShowInventory (Array Oid)
  | Take          (Array Oid)
  | Use           (Array Oid)
  | Examine       (Array Oid)
  | TalkTo String (Action Unit)
  | Say    String (Action Unit)
  | InventoryO Oid
  | ExamineO   Oid
  | TakeO      Oid
  | UseO       Oid
  | UseWith    Oid Oid

instance showOption :: Show Option where
  show _ = "Option"

initOptions ∷ State Story (Array Option)
initOptions = do
  story ← get
  let inventory      = fromFoldable story.player.inventory
  room               ← currentRoom
  itemsToPickUp      ← map fromFoldable $ L.filterM (\oid -> (toObject oid >>= pure <<< _.canPickUp)) room.items
  let itemsToUse     = fromFoldable $ {-inventory <> -} {-L.filter (isLeft <<< _.use <<< toObject state.story)-} room.items
      itemsToExamine = fromFoldable room.items
  itemsInRoom        ← traverse toObject $ fromFoldable room.items
  let toTalkTo       = concatMap (\o → case o.talk of
                                         Just atn → [TalkTo o.title atn]
                                         Nothing  → []
                                 ) itemsInRoom
  pure $  (if null inventory      then [] else [ShowInventory inventory     ])
       <> (if null itemsToPickUp  then [] else [Take          itemsToPickUp ])
       <> (if null itemsToUse     then [] else [Use           itemsToUse    ])
       <> (if null itemsToExamine then [] else [Examine       itemsToExamine])
       <> toTalkTo

addText
  ∷ Array String
  → SS Unit
addText txts =
  let UiState ui = mempty
  in tell $ UiState ui { txts = txts }

clearText
  ∷ SS Unit
clearText =
  let UiState ui = mempty
  in tell $ UiState ui { clear = true }

setOptions
  ∷ Array Option
  → SS Unit
setOptions options =
  let UiState ui = mempty
  in tell $ UiState ui { options = options }

resetOptions
  ∷ SS Unit
resetOptions =
  hoistG initOptions >>= setOptions
