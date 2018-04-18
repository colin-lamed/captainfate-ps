module Motor.Interpreter.ObjectInterpreter
  ( buildObject
  ) where

import Prelude
import Control.Comonad.Store (class ComonadStore, Store, store, seeks)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Proact.Comonad.Trans.Cofree (CofreeT, coiterT)

import Coproduct ((*:*))
import Motor.Story.Lens
import Motor.Story.Types
import Pairing (pairEffect)


coSetOTitle ∷ ∀ w a
            . ComonadStore Object w
            ⇒ w a
            → CoSetOTitleF (w a)
coSetOTitle w = CoSetOTitle $ \title →
  seeks (oTitle .~ title) w

coSetONounType ∷ ∀ w a
               . ComonadStore Object w
               ⇒ w a
               → CoSetONounTypeF (w a)
coSetONounType w = CoSetONounType $ \nounType →
  seeks (oNounType .~ nounType) w

coSetOIsPlural ∷ ∀ w a
               . ComonadStore Object w
               ⇒ w a
               → CoSetOIsPluralF (w a)
coSetOIsPlural w = CoSetOIsPlural $ \isPlural →
  seeks (oIsPlural .~ isPlural) w

coSetODescr ∷ ∀ w a
            . ComonadStore Object w
            ⇒ w a
            → CoSetODescrF (w a)
coSetODescr w = CoSetODescr $ \action →
  seeks (oDescr .~ action) w

coSetOCanPickUp ∷ ∀ w a
                . ComonadStore Object w
                ⇒ w a
                → CoSetOCanPickUpF (w a)
coSetOCanPickUp w = CoSetOCanPickUp $ \canPickUp →
  seeks (oCanPickUp .~ canPickUp) w

coSetOUse ∷ ∀ w a
          . ComonadStore Object w
          ⇒ w a
          → CoSetOUseF (w a)
coSetOUse w = CoSetOUse $ \useAction →
  seeks (oUse .~ useAction) w

coSetOTalk ∷ ∀ w a
           . ComonadStore Object w
           ⇒ w a
           → CoSetOTalkF (w a)
coSetOTalk w = CoSetOTalk $ \action →
  seeks (oTalk ?~ action) w


type Stack = Store Object
type ObjectBuilderInterpreter a = CofreeT CoObjectBuilderF Stack a

mkCofree ∷ ∀ a
          . Stack a
         → ObjectBuilderInterpreter a
mkCofree =
  coiterT (   coSetOTitle
          *:* coSetONounType
          *:* coSetOIsPlural
          *:* coSetODescr
          *:* coSetOCanPickUp
          *:* coSetOUse
          *:* coSetOTalk
          )

interpret ∷ ∀ a r c
          . (a → r → c)
          → ObjectBuilderInterpreter a
          → ObjectBuilder r
          → c
interpret f interpreter =
    unwrap <<< pairEffect f interpreter


buildObject ∷ ∀ r
            . ObjectBuilder r → Object
buildObject objectBuilder =
  let start                    ∷ Stack Object
      start                    = store id $
                                   { title    : "1"-- unsafeCrashWith "title - not set"
                                   , descr    : pure unit
                                   , nounType : Particular
                                   , isPlural : false
                                   , canPickUp: false
                                   , use      : Right (pure unit)
                                   , talk     : Nothing
                                   }
      objectBuilderInterpreter = mkCofree start
  in interpret (\l _ → l) objectBuilderInterpreter objectBuilder
