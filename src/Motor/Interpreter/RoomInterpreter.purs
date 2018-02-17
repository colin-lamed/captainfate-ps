module Motor.Interpreter.RoomInterpreter
  ( buildRoom
  ) where

import Prelude
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (State, modify, execState)
import Control.Plus (empty)
import Partial.Unsafe (unsafeCrashWith)
import Motor.Story


interpret ∷ ∀ next. RoomBuilderF (RoomBuilder next) → State Room (RoomBuilder next)
interpret (SetRTitle title a) = do
  modify \r → r { title = title }
  pure a
interpret (SetRDescr atn a) = do
  modify \r → r { descr = atn }
  pure a
interpret (SetRExits exit a) = do
  modify \r → r { exitsBuilder = exit }
  pure a
interpret (SetRItems oids a) = do
  modify \r → r { items = oids }
  pure a



buildRoom ∷ RoomBuilder Unit → Room
buildRoom rb = execState (runFreeM interpret rb) initialRoom
  where initialRoom = { title        : "1"-- unsafeCrashWith "title - not set"
                      , descr        : pure unit -- unsafeCrashWith "title - not set"
                      , exitsBuilder : pure unit
                      , items        : empty
                      }
