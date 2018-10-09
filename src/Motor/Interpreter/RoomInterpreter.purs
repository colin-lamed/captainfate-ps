module Motor.Interpreter.RoomInterpreter
  ( buildRoom
  ) where

import Prelude
import Control.Comonad.Cofree.Trans (CofreeT, coiterT)
import Control.Comonad.Store (class ComonadStore, Store, store, seeks)
import Control.Plus (empty)
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (pairEffect)
import Data.Functor.Product.Infix ((*:*), (>:<))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)
import Record (merge)

import Motor.Story.Types


coSetRTitle
  ∷ ∀ w a
  . ComonadStore TempRoom w
  ⇒ w a
  → CoSetRTitleF (w a)
coSetRTitle w = CoSetRTitle $ \title →
  seeks (_ { title = Just title}) w

coSetRDescr
  ∷ ∀ w a
  . ComonadStore TempRoom w
  ⇒ w a
  → CoSetRDescrF (w a)
coSetRDescr w = CoSetRDescr $ \action →
  seeks (_ { descr = Just action}) w

coSetRExits
  ∷ ∀ w a
  . ComonadStore TempRoom w
  ⇒ w a
  → CoSetRExitsF (w a)
coSetRExits w = CoSetRExits $ \roomBuilder →
  seeks (_ { exitsBuilder = roomBuilder}) w

coSetRItems
  ∷ ∀ w a
  . ComonadStore TempRoom w
  ⇒ w a
  → CoSetRItemsF (w a)
coSetRItems w = CoSetRItems $ \oids →
  seeks (_ { items = oids }) w


-- same as Room, but with Maybe for required fields
type TempRoom =
  { title        ∷ Maybe String
  , descr        ∷ Maybe (Action Unit)
  , exitsBuilder ∷ ExitsBuilder Unit
  , items        ∷ Array Oid
  }
type Stack                    = Store TempRoom
type RoomBuilderInterpreter a = CofreeT CoRoomBuilderF Stack a

mkCofree
  ∷ ∀ a
  . Stack a
  → RoomBuilderInterpreter a
mkCofree =
  coiterT (   coSetRTitle
          *:* coSetRDescr
          *:* coSetRExits
          *:* coSetRItems
          )

interpret
  ∷ ∀ a r c
  . (a → r → c)
  → RoomBuilderInterpreter a
  → RoomBuilder r
  → c
interpret f interpreter =
    unwrap <<< pairEffect roomBuilderPairing f interpreter

roomBuilderPairing ∷ CoRoomBuilderF ⋈ RoomBuilderF
roomBuilderPairing = setRTitlePairing
                  >:< setRDescrPairing
                  >:< setRExitsPairing
                  >:< setRItemsPairing


buildRoom
  ∷ ∀ r
  . RoomBuilder r
  → Room
buildRoom roomBuilder =
  let start ∷ Stack TempRoom
      start = store identity
               { title        : Nothing
               , descr        : Nothing
               , exitsBuilder : pure unit
               , items        : empty
               }
      interpreter = mkCofree start
      tempRoom = interpret (\l _ → l) interpreter roomBuilder
  in merge { title : fromMaybe' (\_ -> unsafeCrashWith "title not defined") tempRoom.title }
   $ merge { descr : fromMaybe' (\_ -> unsafeCrashWith "desc not defined")  tempRoom.descr }
   $ tempRoom
