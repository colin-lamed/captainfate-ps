module Motor.Interpreter.UseActionInterpreter
  ( runUseAction
  ) where

import Prelude (Unit, bind, discard, pure, ($), (==))
import Control.Monad.Free (runFreeM)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Maybe.First (First(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Motor.Interpreter.StateInterpreter (interpretGetState)
import Motor.Story.Types (Action, Oid, Story, UseAction, UseActionF(..))


interpret ∷ ∀ next. Oid → UseActionF (UseAction next) → StateT Story (Writer (First (Action Unit))) (UseAction next)
interpret oid (With oid' atn a) = do
  tell $ First $ if oid == oid' then Just atn else Nothing
  pure a
interpret _ (GetState1 exists) = interpretGetState exists

runUseAction ∷ ∀ m. MonadState Story m ⇒ UseAction Unit → Oid → m (Maybe (Action Unit))
runUseAction action oid = do
  story ← get
  let Tuple s (First w) = runWriter $ execStateT (runFreeM (interpret oid) action) story
  put s
  pure w
