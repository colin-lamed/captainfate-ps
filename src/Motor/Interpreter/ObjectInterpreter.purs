module Motor.Interpreter.ObjectInterpreter
  ( buildObject
  ) where

import Prelude
import Control.Comonad.Cofree.Trans (CofreeT, coiterT)
import Control.Comonad.Store (class ComonadStore, Store, store, seeks)
import Data.Either (Either(..))
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (pairEffect)
import Data.Functor.Product.Infix ((*:*), (>:<))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)
import Record (merge)

import Motor.Story.Types


coSetOTitle
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetOTitleF (w a)
coSetOTitle w = CoSetOTitle $ \title →
  seeks (_ { title = Just title }) w

coSetONounType
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetONounTypeF (w a)
coSetONounType w = CoSetONounType $ \nounType →
  seeks (_ { nounType = nounType }) w

coSetOIsPlural
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetOIsPluralF (w a)
coSetOIsPlural w = CoSetOIsPlural $ \isPlural →
  seeks (_ { isPlural = isPlural }) w

coSetODescr
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetODescrF (w a)
coSetODescr w = CoSetODescr $ \action →
  seeks (_ { descr = Just action }) w

coSetOCanPickUp
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetOCanPickUpF (w a)
coSetOCanPickUp w = CoSetOCanPickUp $ \canPickUp →
  seeks (_ { canPickUp = canPickUp }) w

coSetOUse
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetOUseF (w a)
coSetOUse w = CoSetOUse $ \useAction →
  seeks (_ { use = useAction }) w

coSetOTalk
  ∷ ∀ w a
  . ComonadStore TempObject w
  ⇒ w a
  → CoSetOTalkF (w a)
coSetOTalk w = CoSetOTalk $ \action →
  seeks (_ { talk = Just action }) w


-- same as Object, but with Maybe for required fields
type TempObject =
  { title     ∷ Maybe String
  , nounType  ∷ NounType
  , isPlural  ∷ Boolean
  , descr     ∷ Maybe (Action Unit)
  , canPickUp ∷ Boolean
  , use       ∷ Either (Action Unit) (UseAction Unit)
  , talk      ∷ Maybe (Action Unit)
  }
type Stack = Store TempObject
type ObjectBuilderInterpreter a = CofreeT CoObjectBuilderF Stack a

mkCofree
  ∷ ∀ a
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

interpret
  ∷ ∀ a r c
  . (a → r → c)
  → ObjectBuilderInterpreter a
  → ObjectBuilder r
  → c
interpret f interpreter =
    unwrap <<< pairEffect objectBuilderPairing f interpreter

objectBuilderPairing ∷ CoObjectBuilderF ⋈ ObjectBuilderF
objectBuilderPairing = setOTitlePairing
                    >:< setONounTypePairing
                    >:< setOIsPluralPairing
                    >:< setODescrPairing
                    >:< setOCanPickUpPairing
                    >:< setOUsePairing
                    >:< setOTalkPairing


buildObject
  ∷ ∀ r
  . ObjectBuilder r
  → Object
buildObject objectBuilder =
  let start ∷ Stack TempObject
      start = store identity
               { title    : Nothing
               , descr    : Nothing
               , nounType : Particular
               , isPlural : false
               , canPickUp: false
               , use      : Right (pure unit)
               , talk     : Nothing
               }
      interpreter = mkCofree start
      tempObject = interpret (\l _ → l) interpreter objectBuilder
  in merge { title : fromMaybe' (\_ -> unsafeCrashWith "title not defined") tempObject.title }
   $ merge { descr : fromMaybe' (\_ -> unsafeCrashWith "desc not defined")  tempObject.descr }
   $ tempObject
