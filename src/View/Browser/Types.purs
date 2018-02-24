module View.Browser.Types where

import Prelude (class Applicative, class Apply, class Bind, class Functor,
  class Monad, class Semigroup, class Show, Unit, apply, bind, discard, pure, show, void, ($), (<<<), (<>), (>>=), (||))
import Browser.WebStorage (WEB_STORAGE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.State.Trans (StateT, runStateT, get)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DOM (DOM())
import React (ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, Read, Write, readState, writeState)
import Motor.Story.Lens (sInventory, (^.))
import Motor.Story.Types (Action, Oid, Story)
import Motor.Util (currentRoom, toObject)


data UiState = UiState { options ∷ Array Option
                       , txts    ∷ Array String
                       , clear   ∷ Boolean
                       }
instance showUiState ∷ Show UiState where
  show (UiState ui) = "UiState { " <> show ui.options <> "," <> show ui.txts <> "," <> show ui.clear <> " }"


instance semigroupUiState ∷ Semigroup UiState where
  append (UiState ui1) (UiState ui2) =
    UiState { options : ui1.options <> ui2.options
            , txts    : ui1.txts    <> ui2.txts
            , clear   : ui1.clear   || ui2.clear
            }

instance monoidUiState ∷ Monoid UiState where
  mempty = UiState { options : [], txts : [], clear : false }

type AppState = { story ∷ Story
                , ui    ∷ { options ∷ Array Option
                          , txt     ∷ Array String
                          , newTxt  ∷ Array String
                          }
                }

newtype SS a = SS (WriterT UiState (StateT Story (Eff ( console    ∷ CONSOLE
                                                      , dom        ∷ DOM
                                                      , props      ∷ ReactProps
                                                      , refs       ∷ ReactRefs ReadOnly
                                                      , state      ∷ ReactState ReadWrite
                                                      , webStorage ∷ WEB_STORAGE
                                                      ))) a)

derive instance newtypeSS ∷ Newtype (SS a) _

derive instance functorSS ∷ Functor SS

instance applySS ∷ Apply SS where
  apply (SS f) (SS a) = SS $ apply f a

instance applicativeSS ∷ Applicative SS where
  pure = SS <<< pure

instance bindSS ∷ Bind SS where
  bind (SS m) f = let g = unwrap <<< f
                  in SS $ bind m g

instance monadSS ∷ Monad SS

instance monadStateSS ∷ MonadState Story SS where
  state = SS <<< lift <<< state

instance monadTellSS ∷ MonadTell UiState SS where
  tell = SS <<< tell

instance monadEffSS ∷ MonadEff ( console     ∷ CONSOLE
                                , dom        ∷ DOM
                                , props      ∷ ReactProps
                                , refs       ∷ ReactRefs (read ∷ Read)
                                , state      ∷ ReactState (read ∷ Read, write ∷ Write)
                                , webStorage ∷ WEB_STORAGE
                                ) SS where
  liftEff = SS <<< liftEff


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
  Tuple (Tuple a (UiState ui)) story' ← runStateT (runWriterT (unwrap ssa)) state.story
  void $ writeState ctx state { story = story'
                              , ui    = { options : ui.options
                                        , txt     : if ui.clear then [] else state.ui.txt <> state.ui.newTxt
                                        , newTxt  : ui.txts
                                        }
                              }
  pure a


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

instance showOption ∷ Show Option where
  show _ = "Option"

initOptions ∷ ∀ m. MonadState Story m ⇒ m (Array Option)
initOptions = do
  story ← get
  let inventory      = story ^. sInventory
  room               ← currentRoom
  itemsToPickUp      ← A.filterA (\oid → (toObject oid >>= pure <<< _.canPickUp)) room.items
  let itemsToUse     = {-inventory <> -} {-L.filter (isLeft <<< _.use <<< toObject state.story)-} room.items
      itemsToExamine = room.items
  itemsInRoom        ← traverse toObject room.items
  let toTalkTo       = A.concatMap (\o → case o.talk of
                                           Just atn → [TalkTo o.title atn]
                                           Nothing  → []
                                   ) itemsInRoom
  pure $  (if A.null inventory      then [] else [ShowInventory inventory     ])
       <> (if A.null itemsToPickUp  then [] else [Take          itemsToPickUp ])
       <> (if A.null itemsToUse     then [] else [Use           itemsToUse    ])
       <> (if A.null itemsToExamine then [] else [Examine       itemsToExamine])
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
  initOptions >>= setOptions
