module Motor.Interpreter.StoryInterpreter
  ( buildStory
  ) where

import Prelude
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (State, modify, execState, get)
import Control.Plus (empty)
import Data.Exists (runExists)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Motor.Story
import Motor.Interpreter.RoomInterpreter (buildRoom)
import Motor.Interpreter.ObjectInterpreter (buildObject)


interpret ∷ ∀ next. StoryBuilderF (StoryBuilder next) → State Story (StoryBuilder next)
interpret (SetSTitle title a) = do
  modify \s → s { title = title }
  pure a
interpret (MkPlayer os rid a) = do
  modify \s → s { player = { inventory: os, location: rid } }
  pure a
interpret (SetMaxScore i a) = do
  modify \s → s { maxScore = Just i }
  pure a
interpret (MkRoom rid rb a) = do
  let room = buildRoom rb
  modify \s → s { rooms = M.insert rid room s.rooms }
  pure a
interpret (MkObject oid ob a) = do
  let object = buildObject ob
  modify \s → s { objects = M.insert oid object s.objects }
  pure a
interpret (SetSInit atn a) = do
  modify \s → s { init = atn }
  pure a
interpret (MkState key exists) = do
  let {dyn, a} = runExists (\(MkStateF {val, toDyn, next}) →
                             {dyn: toDyn val, a: next (Sid key)}
                           ) exists
  modify \s → s { states = M.insert key dyn s.states }
  pure a




buildStory ∷ StoryBuilder Unit → Story
buildStory sb = execState (runFreeM interpret sb) initialStory
  where initialStory = { title   : "1"-- unsafeCrashWith "title - not set" (needs to be made lazy: Data.Lazy (defer, force)) -- TODO better yet, use Data.Record.Builder (can we prove final result has all expected fields?)
                       , player  : { inventory: empty, location: Rid "madeup" } -- unsafeCrashWith "player - not set"
                       , rooms   : M.empty
                       , objects : M.empty
                       , states  : M.empty
                       , score   : 0
                       , maxScore: Nothing
                       , say     : []
                       , init    : pure unit
                       }
