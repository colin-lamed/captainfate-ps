module Motor.Interpreter.ActionInterpreter
  ( runAction
  , runAction'
  ) where

import Prelude
import Control.Monad (class Monad)
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, State, modify, execStateT, get, put, runStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Exists (Exists, runExists)
import Data.Map as M
import Data.List as L
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Motor.Interpreter.StateInterpreter (interpretGetState)
import Motor.Lens
import Motor.Story
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (class Semigroup)
import Data.Monoid (class Monoid)


roomOf' ∷ Story → Oid → Maybe Rid
roomOf' story oid = L.head $ M.keys $ M.filter (\room → oid `L.elem` room.items) story.rooms

interpret ∷ ∀ next. ActionSyntax (Action next) → StateT Story (Writer (Array String)) (Action next)
interpret (PrintLn str a) = do
  tell [ str ]
  pure a
interpret (AddItem rid oid a) = do
  modify $ (sRooms <<< at rid <<< _Just <<< rItems) %~ (oid L.: _)
  pure a
interpret (TakeItem oid a) = do
  modify $ (sPlayer <<< pInventory) %~ (oid L.: _)
  pure a
interpret (DestroyItem oid a) = do
  modify $ (sPlayer <<< pInventory) %~ L.filter (_ /= oid)
  story ← get
  case roomOf' story oid of
    Nothing  → pure unit
    Just rid → modify $ (sRooms <<< at rid <<< _Just <<< rItems) %~ L.filter (_ /= oid)
  pure a
interpret (IncScore i a) = do
  modify $ sScore %~ (_ + i)
  pure a
-- TODO avoid storying in model? make [(Say,Atn)] the result?
interpret (Say l atn a) = do
  modify $ sSay %~ (_ `A.snoc` (Tuple l atn))
  pure a
interpret (PlayerHas oid a) = do
  story ← get
  let has = oid `L.elem` story.player.inventory
  pure (a has)
interpret (RoomHas rid oid a) = do
  story ← get
  let has = unwrap $ story ^. (sRooms <<< at rid <<< _Just <<< rItems <<< to (Any <<< L.elem oid))
  pure (a has)
interpret (RoomOf oid a) = do
  story ← get
  pure (a (roomOf' story oid))
interpret (CurrentRoom a) = do
  story ← get
  pure (a story.player.location)
interpret (GetState exists) = interpretGetState exists
interpret (SetState exists) = do
  story ← get
  let {sid, dyn, a} = runExists (\(SetStateF { sid: (Sid sid), val, toDyn, next }) →
                       {sid: sid, dyn: toDyn val, a: next}
                     ) exists
  modify $ sStates %~ M.insert sid dyn
  pure a

newtype Any = Any Boolean

instance semigroupAny :: Semigroup Any where
  append (Any x) (Any y) = Any (x || y)

instance monoidAny :: Monoid Any where
  mempty = Any false

derive instance newtypeAny :: Newtype Any _

runAction ∷ Action Unit → State Story (Array String)
runAction action = do
  story ← get
  let Tuple s w = runWriter $ execStateT (runFreeM interpret action) story
  put s
  pure w

runAction' ∷ ∀ r. Action r → State Story (Tuple (Array String) r)
runAction' action = do
  story ← get
  let Tuple (Tuple r story') txt = runWriter $ runStateT (runFreeM interpret action) story
  put story'
  pure $ Tuple txt r
