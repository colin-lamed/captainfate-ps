module Motor.Interpreter.ObjectInterpreter
  ( buildObject
  ) where

import Prelude (Unit, discard, pure, unit)
import Control.Monad.Free (runFreeM)
import Control.Monad.State (State, modify, execState)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Motor.Story (NounType(..), Object, ObjectBuilder, ObjectBuilderF(..))


interpret ∷ ∀ next. ObjectBuilderF (ObjectBuilder next) → State Object (ObjectBuilder next)
interpret (SetOTitle title a) = do
  modify \o → o { title = title }
  pure a
interpret (SetODescr atn a) = do
  modify \o → o { descr = atn }
  pure a
interpret (SetONounType b a) = do
  modify \o → o { nounType = b }
  pure a
interpret (SetOIsPlural b a) = do
  modify \o → o { isPlural = b }
  pure a
interpret (SetOCanPickUp b a) = do
  modify \o → o { canPickUp = b }
  pure a
interpret (SetOUse u a) = do
  modify \o → o { use = u }
  pure a
interpret (SetOTalk atn a) = do
  modify \o → o { talk = Just atn }
  pure a



buildObject ∷ ObjectBuilder Unit → Object
buildObject ob = execState (runFreeM interpret ob) initialObject
  where initialObject = { title    : "1"-- unsafeCrashWith "title - not set"
                        , descr    : pure unit
                        , nounType : Particular
                        , isPlural : false
                        , canPickUp: false
                        , use      : Right (pure unit)
                        , talk     : Nothing
                        }
