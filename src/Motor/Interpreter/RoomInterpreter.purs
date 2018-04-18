module Motor.Interpreter.RoomInterpreter
  ( buildRoom
  ) where

import Prelude
import Control.Comonad.Store (class ComonadStore, Store, store, seeks)
import Control.Plus (empty)
import Data.Newtype (unwrap)
import Proact.Comonad.Trans.Cofree (CofreeT, coiterT)

import Coproduct ((*:*))
import Motor.Story.Lens
import Motor.Story.Types
import Pairing (pairEffect)


coSetRTitle ∷ ∀ w a
            . ComonadStore Room w
            ⇒ w a
           → CoSetRTitleF (w a)
coSetRTitle w = CoSetRTitle $ \title →
  seeks (rTitle .~ title) w

coSetRDescr ∷ ∀ w a
            . ComonadStore Room w
            ⇒ w a
            → CoSetRDescrF (w a)
coSetRDescr w = CoSetRDescr $ \action →
  seeks (rDescr .~ action) w

coSetRExits ∷ ∀ w a
            . ComonadStore Room w
            ⇒ w a
            → CoSetRExitsF (w a)
coSetRExits w = CoSetRExits $ \roomBuilder →
  seeks (rExitsBuilder .~ roomBuilder) w

coSetRItems ∷ ∀ w a
            . ComonadStore Room w
            ⇒ w a
            → CoSetRItemsF (w a)
coSetRItems w = CoSetRItems $ \oids →
  seeks (rItems .~ oids) w



type Stack                    = Store Room
type RoomBuilderInterpreter a = CofreeT CoRoomBuilderF Stack a

mkCofree ∷ ∀ a
          . Stack a
         → RoomBuilderInterpreter a
mkCofree =
  coiterT (   coSetRTitle
          *:* coSetRDescr
          *:* coSetRExits
          *:* coSetRItems
          )

interpret ∷ ∀ a r c
          . (a → r → c)
          → RoomBuilderInterpreter a
          → RoomBuilder r
          → c
interpret f interpreter =
    unwrap <<< pairEffect f interpreter


buildRoom ∷ ∀ r
          . RoomBuilder r → Room
buildRoom roomBuilder =
  let start                  ∷ Stack Room
      start                  = store id $
                                 { title        : "1"-- unsafeCrashWith "title - not set"
                                 , descr        : pure unit -- unsafeCrashWith "title - not set"
                                 , exitsBuilder : pure unit
                                 , items        : empty
                                 }
      roomBuilderInterpreter = mkCofree start
  in interpret (\l _ → l) roomBuilderInterpreter roomBuilder
