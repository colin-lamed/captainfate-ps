module Motor.Interpreter.StoryInterpreter
  ( buildStory
  ) where

import Prelude (Unit, discard, pure, unit, (<<<))
import Control.Monad.Free (runFreeM)
import Control.Monad.State (State, execState)
import Control.Plus (empty)
import Data.Exists (runExists)
import Data.Map as M
import Data.Maybe (Maybe(..))

import Motor.Interpreter.RoomInterpreter (buildRoom)
import Motor.Interpreter.ObjectInterpreter (buildObject)
import Motor.Story.Lens (sInit, sMaxScore, sObjects, sPlayer, sRooms, sStates, pInventory, setLocation, setTitle, (%=), (?=), (.=))
import Motor.Story.Types (MkStateF(..), Sid(..), Story(..), StoryBuilder, StoryBuilderF(..))


interpret ∷ ∀ next. StoryBuilderF (StoryBuilder next) → State Story (StoryBuilder next)
interpret (SetSTitle title a) = do
  setTitle ?= title
  pure a
interpret (MkPlayer os rid a) = do
  sPlayer <<< pInventory .= os
  sPlayer <<< setLocation ?= rid
  pure a
interpret (SetMaxScore i a) = do
  sMaxScore ?= i
  pure a
interpret (MkRoom rid rb a) = do
  let room = buildRoom rb
  sRooms %= M.insert rid room
  pure a
interpret (MkObject oid ob a) = do
  let object = buildObject ob
  sObjects %= M.insert oid object
  pure a
interpret (SetSInit atn a) = do
  sInit .= atn
  pure a
interpret (MkState key exists) = do
  let {dyn, a} = runExists (\(MkStateF {val, toDyn, next}) →
                             {dyn: toDyn val, a: next (Sid key)}
                           ) exists
  sStates %= M.insert key dyn
  pure a




buildStory ∷ StoryBuilder Unit → Story
buildStory sb = execState (runFreeM interpret sb) initialStory
  -- TODO title and player.location are required. Currently, lens will throw exception of not set on first read.
  -- ideally want to not compile story if not set.
  -- next best would be to throw exception after buildStory (e.g. convert temporary model (with Maybe) to Story)

  -- Could not use Data.Record.Builder
  -- e.g. interpret ∷ Writer (RB.Builder a b) (StoryBuilder next)
  --   tell $ insert (SProxy :: SProxy "title") title
  -- since Builder type changes with each insert
  where initialStory = Story { title   : Nothing
                             , player  : { inventory: empty, location: Nothing }
                             , rooms   : M.empty
                             , objects : M.empty
                             , states  : M.empty
                             , score   : 0
                             , maxScore: Nothing
                             , say     : []
                             , init    : pure unit
                             }
