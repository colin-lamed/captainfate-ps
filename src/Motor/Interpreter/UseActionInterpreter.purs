module Motor.Interpreter.UseActionInterpreter
  ( runUseAction
  ) where

import Prelude
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, State, modify, execStateT, get, put, runStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Exists (runExists)
import Data.Map as M
import Data.Maybe.First (First(..))
import Data.List as L
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Motor.Story
import Motor.Interpreter.StateInterpreter (interpretGetState)


interpret ∷ ∀ next. Oid → UseActionF (UseAction next) → StateT Story (Writer (First (Action Unit))) (UseAction next)
interpret oid (With oid' atn a) = do
  tell $ First $ if oid == oid' then Just atn else Nothing
  pure a
interpret _ (GetState1 exists) = interpretGetState exists

runUseAction ∷ UseAction Unit → Oid → State Story (Maybe (Action Unit))
runUseAction action oid = do
  story ← get
  let Tuple s (First w) = runWriter $ execStateT (runFreeM (interpret oid) action) story
  put s
  pure w
