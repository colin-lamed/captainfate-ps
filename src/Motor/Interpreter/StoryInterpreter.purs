module Motor.Interpreter.StoryInterpreter
  ( buildStory
  ) where

import Prelude
import Control.Comonad.Cofree.Trans (CofreeT, coiterT)
import Control.Comonad.Store (class ComonadStore, Store, store, seeks)
import Data.Either (Either(..), note)
import Data.Exists (mkExists)
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (pairEffect)
import Data.Functor.Product.Infix ((*:*), (>:<))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign (Foreign, unsafeToForeign)
import Record (merge)

import Motor.Story.Types
import Motor.Interpreter.RoomInterpreter (buildRoom)
import Motor.Interpreter.ObjectInterpreter (buildObject)



coSetSTitle
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoSetSTitleF (w a)
coSetSTitle w = CoSetSTitle $ \title →
  seeks (_ { title = Just title }) w

coMkPlayer
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoMkPlayerF (w a)
coMkPlayer w = CoMkPlayer $ \inventory location →
  seeks (_ { player = Just { inventory, location } }) w

coMkObject
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoMkObjectF (w a)
coMkObject w = CoMkObject $ \oid ob →
  let eobj = buildObject oid ob
  in seeks (\s -> s { objects = M.insert oid <$> eobj <*> s.objects } ) w

coMkRoom
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoMkRoomF (w a)
coMkRoom w = CoMkRoom $ \rid rb →
  let eRoom = buildRoom rid rb
  in seeks (\s -> s { rooms = M.insert rid <$> eRoom <*> s.rooms }) w

coSetSInit
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoSetSInitF (w a)
coSetSInit w = CoSetSInit $ \a →
  seeks (_ { init = a }) w

coSetMaxScore
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoSetMaxScoreF (w a)
coSetMaxScore w = CoSetMaxScore $ \i →
  seeks (_ { maxScore = Just i}) w

coMkState
  ∷ ∀ w a
  . ComonadStore TempStory w
  ⇒ w a
  → CoMkStateF (w a)
coMkState w =
  let exists = mkExists $ CoMkStateF1 { next : \l val →
        let sid = l -- we could generate label instead of client passing in (e.g. UUID)
        in Tuple (Sid sid) (seeks (\s -> s { states = M.insert sid (unsafeToForeign val) s.states }) w)
      }
  in CoMkState exists



-- same as Story, but with Maybe for required fields
-- and Either for failable fields
type TempStory =
  { title    ∷ Maybe String
  , player   ∷ Maybe Player
  , rooms    ∷ Either String (M.Map Rid Room)
  , objects  ∷ Either String (M.Map Oid Object)
  , states   ∷ M.Map String Foreign
  , score    ∷ Int
  , maxScore ∷ Maybe Int
  , say      ∷ Array (Tuple String (Action Unit))
  , init     ∷ Action Unit
  }
type Stack                     = Store TempStory
type StoryBuilderInterpreter a = CofreeT CoStoryBuilderF Stack a

mkCofree
  ∷ ∀ a
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

interpret
  ∷ ∀ a r c
  . (a → r → c)
  → StoryBuilderInterpreter a
  → StoryBuilder r
  → c
interpret f interpreter dsl =
    unwrap $ pairEffect storyBuilderPairing f interpreter dsl

storyBuilderPairing ∷ CoStoryBuilderF ⋈ StoryBuilderF
storyBuilderPairing =   setSTitlePairing
                    >:< mkPlayerPairing
                    >:< mkObjectPairing
                    >:< mkRoomPairing
                    >:< mkStatePairing
                    >:< setSInitPairing
                    >:< setMaxScorePairing


buildStory ∷ StoryBuilder Unit → Either String Story
buildStory storyBuilder = do
  let start ∷ Stack TempStory
      start = store identity
               { title   : Nothing
               , player  : Nothing
               , rooms   : Right M.empty
               , objects : Right M.empty
               , states  : M.empty
               , score   : 0
               , maxScore: Nothing
               , say     : []
               , init    : pure unit
               }
      interpreter = mkCofree start
      tempStory = interpret (\l _ → l) interpreter storyBuilder
  title   <- note "story title not defined" tempStory.title
  player  <- note "story player not defined" tempStory.player
  rooms   <- tempStory.rooms
  objects <- tempStory.objects
  pure $ Story $ merge { title, player, rooms, objects } tempStory
