module Motor.Interpreter.StoryInterpreter
  ( buildStory
  ) where

import Prelude
import Control.Comonad.Store (class ComonadStore, Store, store, seeks)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Map as M
import Proact.Comonad.Trans.Cofree (CofreeT, coiterT)

import Coproduct ((*:*))
import Motor.Story.Lens
import Motor.Story.Types
import Motor.Interpreter.RoomInterpreter (buildRoom)
import Motor.Interpreter.ObjectInterpreter (buildObject)
import Motor.Interpreter.StateInterpreter (coMkState)
import Pairing (pairEffect)


coSetSTitle ∷ ∀ w a
            . ComonadStore Story w
            ⇒ w a
            → CoSetSTitleF (w a)
coSetSTitle w = CoSetSTitle $ \title →
  seeks (setTitle ?~ title) w

coMkPlayer ∷ ∀ w a
           . ComonadStore Story w
           ⇒ w a
           → CoMkPlayerF (w a)
coMkPlayer w = CoMkPlayer $ \inventory location →
  seeks (sPlayer .~ { inventory : inventory
                    , location  : Just location
                    }) w

coMkObject ∷ ∀ w a
           . ComonadStore Story w
           ⇒ w a
           → CoMkObjectF (w a)
coMkObject w = CoMkObject $ \oid ob →
  let obj = buildObject ob
  in seeks (sObjects %~ (M.insert oid obj)) w

coMkRoom ∷ ∀ w a
         . ComonadStore Story w
         ⇒ w a
         → CoMkRoomF (w a)
coMkRoom w = CoMkRoom $ \rid rb →
  let room = buildRoom rb
  in seeks (sRooms %~ (M.insert rid room)) w

coSetSInit ∷ ∀ w a
           . ComonadStore Story w
           ⇒ w a
           → CoSetSInitF (w a)
coSetSInit w = CoSetSInit $ \a →
  seeks (sInit .~ a) w

coSetMaxScore ∷ ∀ w a
              . ComonadStore Story w
              ⇒ w a
              → CoSetMaxScoreF (w a)
coSetMaxScore w = CoSetMaxScore $ \i →
  seeks (sMaxScore .~ Just i) w


type Stack                     = Store Story
type StoryBuilderInterpreter a = CofreeT CoStoryBuilderF Stack a

mkCofree ∷ ∀ a
          . Stack a
         → StoryBuilderInterpreter a
mkCofree =
  coiterT (   coSetSTitle
          *:* coMkPlayer
          *:* coMkObject
          *:* coMkRoom
          *:* coMkState
          *:* coSetSInit
          *:* coSetMaxScore
          )

interpret ∷ ∀ a r c
          . (a → r → c)
          → StoryBuilderInterpreter a
          → StoryBuilder r
          → c
interpret f interpreter =
    unwrap <<< pairEffect f interpreter


buildStory ∷ StoryBuilder Unit → Story
buildStory storyBuilder =
  let start                   ∷ Stack Story
      start                   = store id $ Story
                                          { title   : Nothing
                                           , player  : { inventory: empty, location: Nothing }
                                           , rooms   : M.empty
                                           , objects : M.empty
                                           , states  : M.empty
                                           , score   : 0
                                           , maxScore: Nothing
                                           , say     : []
                                           , init    : pure unit
                                           }
      storyBuilderInterpreter = mkCofree start
  in interpret (\l _ → l) storyBuilderInterpreter storyBuilder
