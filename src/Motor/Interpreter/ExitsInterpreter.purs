module Motor.Interpreter.ExitsInterpreter
  ( buildExits
  ) where

import Prelude (Unit, bind, discard, pure, ($))
import Control.Monad.Free (runFreeM)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Tuple (Tuple(..))
import Motor.Interpreter.StateInterpreter (interpretGetState)
import Motor.Story.Types (Exit, ExitsBuilder, ExitsBuilderF(..), Story)


interpret ∷ ∀ next. ExitsBuilderF (ExitsBuilder next) → StateT Story (Writer (Array Exit)) (ExitsBuilder next)
interpret (AddExit e a) = do
  tell [e]
  pure a
interpret (GetState2 exists) = interpretGetState exists

buildExits ∷ ∀ m. MonadState Story m ⇒ ExitsBuilder Unit → m (Array Exit)
buildExits builder = do
  story ← get
  let Tuple a w = runWriter $ execStateT (runFreeM interpret builder) story
  put a
  pure w
