module Motor.Interpreter.ExitsInterpreter
  ( buildExits
  ) where

import Prelude
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, State, modify, execStateT, get, put)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Exists (Exists, runExists)
import Data.Map as M
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Motor.Story
import Motor.Interpreter.StateInterpreter (interpretGetState)


interpret ∷ ∀ next. ExitsBuilderF (ExitsBuilder next) → StateT Story (Writer (Array Exit)) (ExitsBuilder next)
interpret (AddExit e a) = do
  tell [e]
  pure a
interpret (GetState2 exists) = interpretGetState exists

buildExits ∷ ExitsBuilder Unit → State Story (Array Exit)
buildExits builder = do
  story ← get
  let Tuple a w = runWriter $ execStateT (runFreeM interpret builder) story
  put a
  pure w
