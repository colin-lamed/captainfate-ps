module Motor.Interpreter.ActionInterpreter
  ( runAction
  , runAction'
  ) where

import Prelude
import Control.Comonad.Cofree.Trans (CofreeT, coiterT)
import Control.Comonad.Store (class ComonadStore, Store, pos, seeks, store)
import Control.Comonad.Traced (class ComonadTraced, TracedT(TracedT), track)
import Control.Extend (duplicate)
import Control.Monad.State (class MonadState, state)
import Data.Array ((:), filter, elem)
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (pairEffect)
import Data.Functor.Product (Product(..))
import Data.Functor.Product.Infix (type (⊕), type (⊗), (*:*), (>:<))
import Data.List as L
import Data.Maybe (Maybe, maybe)
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Set as S
import Data.Tuple (Tuple(..))

import Motor.Story.Lens
import Motor.Story.Types
import Motor.Interpreter.StateInterpreter (coGetState, coSetState)

coPrintLn
  ∷ ∀ w a
  . ComonadTraced (Array String) w
  ⇒ w a
  → CoPrintLnF (w a)
coPrintLn w =
  CoPrintLn $ \txt →
    track [txt] (duplicate w)

coAddItem
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoAddItemF (w a)
coAddItem w =
    CoAddItem $ \rid oid →
      seeks (addItem oid rid) w
  where
    addItem oid rid = (sRooms <<< at rid <<< _Just <<< rItems) <>~ [oid]

coTakeItem
  ∷ ∀ w a
  .  ComonadStore Story w
  ⇒ w a
  → CoTakeItemF (w a)
coTakeItem w =
    CoTakeItem $ \oid →
      seeks (takeItem oid) w
  where
    takeItem oid = sInventory %~ (oid : _)

coDestroyItem
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoDestroyItemF (w a)
coDestroyItem w =
    CoDestroyItem $ \oid →
      seeks (remove oid) w
  where remove oid s = maybe (removeFromInv oid s) (flip (removeFromRoom oid) s) $ roomOf oid s
        removeFromRoom oid rid = (sRooms <<< at rid <<< _Just <<< rItems) %~ (filter (_ /= oid))
        removeFromInv  oid     = sInventory                               %~ (filter (_ /= oid))

coIncScore
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoIncScoreF (w a)
coIncScore w =
  CoIncScore $ \i →
    seeks (sScore %~ (_ + i)) w

-- TODO avoid storing in model? make [(Say,Atn)] the result?
coSay
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoSayF (w a)
coSay w =
    CoSay $ \say atn →
      seeks (addSay say atn) w
  where
    addSay say atn = sSay %~ (_ <> [Tuple say atn])

coPlayerHas
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoPlayerHasF (w a)
coPlayerHas w =
    CoPlayerHas $ \oid →
      Tuple (has oid $ pos w) w
  where
    has oid = (oid `elem` _) <<< (\s → s ^. sInventory)

coRoomHas
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoRoomHasF (w a)
coRoomHas w =
    CoRoomHas $ \rid oid →
      Tuple (has rid oid $ pos w) w
  where
    has rid oid = (oid `elem` _) <<< (\s → s ^. sRooms <<< at rid <<< _Just <<< rItems)

roomOf ∷ Oid → Story → Maybe Rid
roomOf oid s =
    S.findMin rids -- will return the min Oid if more than one match
  where
    rids = M.keys $ M.filter ((oid `elem` _) <<< (_ ^. rItems)) $ s ^. sRooms

coRoomOf
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoRoomOfF (w a)
coRoomOf w =
  CoRoomOf $ \oid →
    Tuple (roomOf oid $ pos w) w

currentRoom ∷ Story → Rid
currentRoom s =
  s ^. sLocation

coCurrentRoom
  ∷ ∀ w a
  . ComonadStore Story w
  ⇒ w a
  → CoCurrentRoomF (w a)
coCurrentRoom w =
  CoCurrentRoom $
    Tuple (currentRoom $ pos w) w


type Stack               = TracedT (Array String) (Store Story)
type ActionInterpreter a = CofreeT CoActionF Stack a

mkCofree
  ∷ ∀ a
  . Stack a
  → ActionInterpreter a
mkCofree =
  coiterT (   coPrintLn
          *:* coAddItem
          *:* coTakeItem
          *:* coDestroyItem
          *:* coIncScore
          *:* coSay
          *:* coPlayerHas
          *:* coRoomHas
          *:* coRoomOf
          *:* coCurrentRoom
          *:* coGetState
          *:* coSetState
          )

interpret
  ∷ ∀ a r c
  . (a → r → c)
  → ActionInterpreter a
  → Action r
  → c
interpret f interpreter =
    unwrap <<< pairEffect actionPairing f interpreter

actionPairing ∷ CoActionF ⋈ ActionF
actionPairing = printLnPairing
              >:< addItemPairing
              >:< takeItemPairing
              >:< destroyItemPairing
              >:< incScorePairing
              >:< sayPairing
              >:< playerHasPairing
              >:< roomHasPairing
              >:< roomOfPairing
              >:< currentRoomPairing
              >:< getStatePairing
              >:< setStatePairing


runAction
  ∷ ∀ m r
  . MonadState Story m
  ⇒ Action r
  → m (Array String)
runAction action =
  state $ \s →
    let start             ∷ Stack (Tuple (Array String) Story)
        start             = TracedT $ store (\s' ts → Tuple ts s') s
        actionInterpreter = mkCofree start
    in interpret (\l _ → l) actionInterpreter action

runAction'
  ∷ ∀ m r
  . MonadState Story m
  ⇒ Action r
  → m (Tuple (Array String) r)
runAction' action =
  state $ \s →
    let start ∷ Stack (Tuple (Array String) Story)
        start = TracedT $ store (\s' ts → Tuple ts s') s
        interpreter = mkCofree start
        Tuple (Tuple txts s') r = interpret (\l r → Tuple l r) interpreter action
    in Tuple (Tuple txts r) s'
