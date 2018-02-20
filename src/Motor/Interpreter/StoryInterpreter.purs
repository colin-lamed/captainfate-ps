module Motor.Interpreter.StoryInterpreter
  ( buildStory
  ) where

import Prelude (Unit, discard, pure, unit, ($))
import Control.Monad.Free (runFreeM)
import Control.Monad.State (State, modify, execState)
import Control.Plus (empty)
import Data.Exists (runExists)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Motor.Story (MkStateF(..), Rid(..), Sid(..), Story(..), StoryBuilder, StoryBuilderF(..))
import Motor.Interpreter.RoomInterpreter (buildRoom)
import Motor.Interpreter.ObjectInterpreter (buildObject)
import Motor.Lens (sInit, sMaxScore, sObjects, sPlayer, sRooms, sStates, sTitle, (%~), (.~))


interpret ∷ ∀ next. StoryBuilderF (StoryBuilder next) → State Story (StoryBuilder next)
interpret (SetSTitle title a) = do
  modify $ sTitle .~ title
  pure a
interpret (MkPlayer os rid a) = do
  modify $ sPlayer .~ { inventory: os, location: rid }
  pure a
interpret (SetMaxScore i a) = do
  modify $ sMaxScore .~ Just i
  pure a
interpret (MkRoom rid rb a) = do
  let room = buildRoom rb
  modify $ sRooms %~ M.insert rid room
  pure a
interpret (MkObject oid ob a) = do
  let object = buildObject ob
  modify $ sObjects %~ M.insert oid object
  pure a
interpret (SetSInit atn a) = do
  modify $ sInit .~ atn
  pure a
interpret (MkState key exists) = do
  let {dyn, a} = runExists (\(MkStateF {val, toDyn, next}) →
                             {dyn: toDyn val, a: next (Sid key)}
                           ) exists
  modify $ sStates %~ M.insert key dyn
  pure a




buildStory ∷ StoryBuilder Unit → Story
buildStory sb = execState (runFreeM interpret sb) initialStory
  where initialStory = Story { title   : "1"-- unsafeCrashWith "title - not set" (needs to be made lazy: Data.Lazy (defer, force)) -- TODO better yet, use Data.Record.Builder (can we prove final result has all expected fields?)
                             , player  : { inventory: empty, location: Rid "madeup" } -- unsafeCrashWith "player - not set"
                             , rooms   : M.empty
                             , objects : M.empty
                             , states  : M.empty
                             , score   : 0
                             , maxScore: Nothing
                             , say     : []
                             , init    : pure unit
                             }
