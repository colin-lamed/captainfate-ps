module Motor.Util where

import Prelude (Unit, bind, discard, map, pure, show, ($), (<<<), (<>), (==), (||), (>>=))
import Control.Alt ((<|>))
import Control.Monad.State (get, evalState)
import Control.Monad.State.Class (class MonadState)
import Data.Array as A
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Map as M
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Foldable as F
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Motor.Interpreter.ActionInterpreter (runAction, runAction')
import Motor.Interpreter.ExitsInterpreter (buildExits)
import Motor.Interpreter.UseActionInterpreter (runUseAction)
import Motor.Story.Lens (at, sInventory, sObjects, sPlayer, sRooms, sSay, sLocation, (.=), (?=), (<>=), use)
import Motor.Story.Types (Action, Exit, NounType(..), Object, Oid, Rid, Room, Story)


toRoom ∷ ∀ m. MonadState Story m ⇒ Rid → m Room
toRoom rid = do
  mRoom ← use $ sRooms <<< at rid
  case mRoom of
    Just r → pure r
    Nothing → unsafeCrashWith ("Room " <> show rid <> " not found")

toObject ∷ ∀ m. MonadState Story m ⇒ Oid → m Object
toObject oid = do
  mObj ← use $ sObjects <<< at oid
  case mObj of
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

listItems ∷ ∀ m. MonadState Story m ⇒ Room → m (Maybe String)
listItems room = do
  items ← traverse toObject room.items
  pure $ case items of
          []     → Nothing
          items' → let item = unsafePartial head items'
                       is  = if item.isPlural then "are "
                                              else "is "
                    in Just $ "There " <> is <> F.intercalate " and " (map a items) <> " here."

listExits ∷ ∀ m. MonadState Story m ⇒ Room → m (Array Exit)
listExits room =
  buildExits room.exitsBuilder


goto ∷ ∀ m. MonadState Story m ⇒ Action (Maybe Rid) → m (Either (Array String) Rid) -- TODO revisit assumption that we either have txt to display, or a new room
goto roomAction = do
  Tuple txts mRid ← runAction' roomAction
  case mRid of
    Just rid → do sLocation .= rid
                  pure $ Right rid
    Nothing  → pure $ Left txts


-- TODO or keep name takeItem - but move helper methods into different module from story builder DSL
takeItemS ∷ ∀ m. MonadState Story m ⇒ Oid → m Unit
takeItemS oid = do
  sInventory <>= [oid]
  -- rId ← currentRoom
  -- (sRooms <<< at rId <<< _Just <<< rItems) %= filter (_ /= oid)

lookupSay ∷ ∀ m. MonadState Story m ⇒ String → m (Either String (Action Unit))
lookupSay say' = do
  sayOptions ← use sSay
  pure case L.find (\(Tuple say _) → say == say') sayOptions of
    Just (Tuple _ atn) → Right atn
    Nothing            → Left ("Could not find say " <> say' <> " options are: " <> (show $ map (\(Tuple say _) → say) sayOptions))


useWith ∷ ∀ m. MonadState Story m ⇒ Oid → Oid → m (Array String)
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
    use' ∷ ∀ m2. MonadState Story m2 ⇒ Object → Oid → m2 (Maybe (Action Unit))
    use' obj1' oid2' =
      case obj1'.use of
        Left  _ → pure Nothing
        Right u → runUseAction u oid2'

useItself ∷ ∀ m. MonadState Story m ⇒ Oid → m (Either String (Array String))
useItself oid = do
  obj ← toObject oid
  case obj.use of
    Left  action → map Right $ runAction action
    Right _      → pure $ Left ("Cannot use " <> show oid <> " with itself") -- only likely when replaying history for changed story

roomDesc ∷ ∀ m. MonadState Story m ⇒ m (Array String)
roomDesc = do
  room  ← currentRoom
  runAction room.descr

lookupRoomAction ∷ ∀ m. MonadState Story m ⇒ String → m (Either String (Action (Maybe Rid)))
lookupRoomAction label = do
  room  ← currentRoom
  exits ← buildExits room.exitsBuilder
  pure case A.find (\e → e.label == label) exits of
    Just exit → Right exit.rid
    Nothing   → Left ("Could not find " <> label <> " from " <> room.title <> " exits are: " <> (show $ map _.label exits))

visible ∷ ∀ m. MonadState Story m ⇒ Tuple Oid Object → m Boolean
visible (Tuple oid _) = do
  inventory ← use sInventory
  room      ← currentRoom
  let inRoom       = oid `A.elem` room.items
      inPossession = oid `A.elem` inventory
  pure $ inRoom || inPossession

lookupOidByTitle ∷ ∀ m. MonadState Story m ⇒ String → m (Either String Oid)
lookupOidByTitle title = do
  visibleObjects ← use sObjects >>= A.filterA visible <<< M.toUnfoldable
  let vosWithTitle ∷ Array (Tuple Oid Object)
      vosWithTitle  = A.filter (\(Tuple _ o) → o.title == title) visibleObjects
      visibleTitles = map (\(Tuple _ o) → o.title) visibleObjects
  pure case A.head vosWithTitle of
         Nothing          → Left ("no " <> title <> " visible (only: " <> show visibleTitles <> ")")
         Just (Tuple l _) → Right l

currentRoom ∷ ∀ m. MonadState Story m ⇒ m Room
currentRoom =
  use sLocation >>= toRoom
