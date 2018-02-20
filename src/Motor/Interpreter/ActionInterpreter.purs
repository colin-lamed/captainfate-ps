module Motor.Interpreter.ActionInterpreter
  ( runAction
  , runAction'
  ) where

import Prelude (class Semigroup, Unit, bind, discard, pure, unit, ($), (+), (/=), (<<<), (||))
import Control.Monad.Free (runFreeM)
import Control.Monad.State (StateT, modify, execStateT, get, put, runStateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Exists (runExists)
import Data.Map as M
import Data.List as L
import Data.Array ((:))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Motor.Interpreter.StateInterpreter (interpretGetState)
import Motor.Lens (_Just, at, pLocation, rItems, sInventory, sPlayer, sRooms, sSay, sScore, sStates, to, (%~), (^.))
import Motor.Story (Action, ActionSyntax(..), Oid, Rid, SetStateF(..), Sid(..), Story)
import Data.Newtype (class Newtype, unwrap)
import Data.Monoid (class Monoid)


roomOf' ∷ Story → Oid → Maybe Rid
roomOf' story oid = L.head $ M.keys $ M.filter (\room → oid `L.elem` room.items) (story ^. sRooms)

interpret ∷ ∀ next. ActionSyntax (Action next) → StateT Story (Writer (Array String)) (Action next)
interpret (PrintLn str a) = do
  tell [ str ]
  pure a
interpret (AddItem rid oid a) = do
  modify $ (sRooms <<< at rid <<< _Just <<< rItems) %~ (oid : _)
  pure a
interpret (TakeItem oid a) = do
  modify $ sInventory %~ (oid : _)
  pure a
interpret (DestroyItem oid a) = do
  modify $ sInventory %~ A.filter (_ /= oid)
  story ← get
  case roomOf' story oid of
    Nothing  → pure unit
    Just rid → modify $ (sRooms <<< at rid <<< _Just <<< rItems) %~ A.filter (_ /= oid)
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
  let has = unwrap $ story ^. (sInventory <<< to (Any <<< A.elem oid))
  pure (a has)
interpret (RoomHas rid oid a) = do
  story ← get
  let has = unwrap $ story ^. (sRooms <<< at rid <<< _Just <<< rItems <<< to (Any <<< A.elem oid))
  pure (a has)
interpret (RoomOf oid a) = do
  story ← get
  pure (a (roomOf' story oid))
interpret (CurrentRoom a) = do
  story ← get
  pure (a (story ^. (sPlayer <<< pLocation)))
interpret (GetState exists) = interpretGetState exists
interpret (SetState exists) = do
  story ← get
  let {sid, dyn, a} = runExists (\(SetStateF { sid: (Sid sid), val, toDyn, next }) →
                       {sid: sid, dyn: toDyn val, a: next}
                     ) exists
  modify $ sStates %~ M.insert sid dyn
  pure a

newtype Any = Any Boolean

instance semigroupAny ∷ Semigroup Any where
  append (Any x) (Any y) = Any (x || y)

instance monoidAny ∷ Monoid Any where
  mempty = Any false

derive instance newtypeAny ∷ Newtype Any _

runAction ∷ ∀ m. MonadState Story m ⇒ Action Unit → m (Array String)
runAction action = do
  story ← get
  let Tuple s w = runWriter $ execStateT (runFreeM interpret action) story
  put s
  pure w

runAction' ∷ ∀ m r. MonadState Story m ⇒ Action r → m (Tuple (Array String) r)
runAction' action = do
  story ← get
  let Tuple (Tuple r story') txt = runWriter $ runStateT (runFreeM interpret action) story
  put story'
  pure $ Tuple txt r
