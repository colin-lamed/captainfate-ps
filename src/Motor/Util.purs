module Motor.Util where

import Prelude
import Control.Alt ((<|>))
import Control.Monad ((>>=))
import Control.Monad.State (State, get, modify, put, runState, evalState)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as M
import Data.List ((:))
import Data.List as L
import Data.Maybe
import Data.Foldable as F
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Motor.Story
import Partial.Unsafe (unsafeCrashWith)
import Motor.Interpreter.ActionInterpreter (runAction, runAction')
import Motor.Interpreter.ExitsInterpreter (buildExits)
import Motor.Interpreter.UseActionInterpreter (runUseAction)
import Motor.Lens


toRoom ∷ Rid → State Story Room
toRoom rid = do
  story ← get
  case story ^. (sRooms <<< at rid) of
    Just r → pure r
    Nothing → unsafeCrashWith ("Room " <> show rid <> " not found")

toObject ∷ Oid → State Story Object
toObject oid = do
  story ← get
  case story ^. (sObjects <<< at oid) of
    Just o  → pure o
    Nothing → unsafeCrashWith ("Object " <> show oid <> " not found")

a ∷ Object → String
a obj = pronoun <> " " <> obj.title
  where
    pronoun = case obj.nounType of
      Proper     → ""
      Quantitive → "some"
      Particular → if startsWithVowel obj.title then "an" else "a"
    startsWithVowel = go <<< A.head <<< toCharArray
      where
        go Nothing = false
        go (Just c) = c `A.elem` ['a','e','i','o','u']

the ∷ Object → String
the obj = pronoun <> " " <> obj.title
  where
    pronoun = case obj.nounType of
      Proper     → ""
      Quantitive → "the"
      Particular → "the"


listExits ∷ Room → State Story (Array Exit)
listExits room =
  buildExits room.exitsBuilder


goto ∷ Action (Maybe Rid) → State Story (Either (Array String) Rid) -- TODO revisit assumption that we either have txt to display, or a new room
goto roomAction = do
  story ← get
  Tuple txts mRid ← runAction' roomAction
  case mRid of
    Just rid → do modify $ (sPlayer <<< pLocation) .~ rid
                  pure $ Right rid
    Nothing  → pure $ Left txts


-- TODO or keep name takeItem - but move helper methods into different module from story builder DSL
takeItemS ∷ Oid → State Story Unit
takeItemS oid = do
  story ← get
  let rId = story.player.location
  modify $ (sPlayer <<< pInventory)  %~ (oid : _)
  -- modify $ (sRooms <<< at rId <<< _Just <<< rItems) %~ filter (_ /= oid)

lookupSay :: String → State Story (Either String (Action Unit))
lookupSay say' = do
  story ← get
  pure case L.find (\(Tuple say _) → say == say') story.say of
    Just (Tuple _ atn) → Right atn
    Nothing            → Left ("Could not find say " <> say' <> " options are: " <> (show $ map (\(Tuple say _) → say) story.say))


use ∷ Oid → Maybe Oid → State Story (Array String)
use oid1 mOid2 =
  case mOid2 of
    Just oid2 → useWith oid1 oid2
    Nothing   → useItself oid1

useWith ∷ Oid → Oid → State Story (Array String)
useWith oid1 oid2 = do
    obj1  ← toObject oid1
    obj2  ← toObject oid2
    story ← get
    -- since we use alt, runUseAction cannot update story (we need to backtrack)
    -- (DSL only allows getState, but to avoid evalState here, maybe we should not use alt,
    -- and use convention of only define use a with b on a?)
    let mAction =   evalState (use' obj2 oid1) story
                <|> evalState (use' obj1 oid2) story
    case mAction of
      Nothing     → pure []
      Just action → runAction action
  where
    use' ∷ Object → Oid → State Story (Maybe (Action Unit))
    use' obj1' oid2' =
      case obj1'.use of
        Left  _ → pure Nothing
        Right u → runUseAction u oid2'

useItself ∷ Oid → State Story (Array String)
useItself oid = do
    obj ← toObject oid
    let action = fromLeft $ obj.use
    runAction action
  where fromLeft ∷ ∀ a b. Either a b → a
        fromLeft (Left  a) = a
        fromLeft (Right _) = unsafeCrashWith "Not right..."

roomDesc ∷ State Story (Array String)
roomDesc = do
  room  ← currentRoom
  runAction room.descr

lookupRoomAction ∷ String → State Story (Either String (Action (Maybe Rid)))
lookupRoomAction exit = do
  room  ← currentRoom
  exits ← buildExits room.exitsBuilder
  pure case A.find (\e → e.label == exit) exits of
    Just exit → Right exit.rid
    Nothing   → Left ("Could not find " <> exit <> " from " <> room.title <> " exits are: " <> (show $ map _.label exits))

visible ∷ Tuple Oid Object → State Story Boolean
visible (Tuple oid _) = do
  story ← get
  room  ← currentRoom
  let inRoom       = oid `A.elem` room.items
      inPossession = oid `A.elem` story.player.inventory
  pure $ inRoom || inPossession

lookupOidByTitle ∷ String → State Story (Either String Oid)
lookupOidByTitle title = do
  story ← get
  visibleObjects ← L.filterM visible $ M.toUnfoldable $ story.objects
  let vosWithTitle :: Array (Tuple Oid Object)
      vosWithTitle  = A.filter (\(Tuple _ o) → o.title == title) $ A.fromFoldable visibleObjects
      visibleTitles = map (\(Tuple _ o) → o.title) visibleObjects
  pure case A.head vosWithTitle of
         Nothing          → Left ("no " <> title <> " visible (only: " <> show visibleTitles <> ")")
         Just (Tuple l _) → Right l

currentRoom ∷ State Story Room
currentRoom = do
  story ← get
  toRoom story.player.location
