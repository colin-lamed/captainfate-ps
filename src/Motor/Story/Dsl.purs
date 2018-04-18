module Motor.Story.Dsl
  ( class GetStateAction
  , class RidAction
  , class SetUse
  , action
  , addItem
  , currentRoom
  , destroyItem
  , exit
  , getState
  , getState2
  , incScore
  , mkObject
  , mkPlayer
  , mkRoom
  , mkState
  , playerHas
  , printLn
  , roomHas
  , roomOf
  , say
  , setMaxScore
  , setOCanPickUp
  , setODescr
  , setOIsPlural
  , setONounType
  , setOTitle
  , setOUse
  , setRDescr
  , setRExits
  , setRItems
  , setRTitle
  , setSInit
  , setState
  , setTitle
  , takeItem
  , talkO
  , toAction
  , toSetUse
  , with
  ) where

import Prelude (Unit, id, pure, unit, ($), (<<<))
import Proact.Monad.Trans.Free (FreeT)
import Proact.Monad.Class.MonadFree (liftFree)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Dynamic (toDynamic, fromDynamic)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Typeable (class Typeable)
import Coproduct (inj, type (⊕))
import Motor.Story.Types


setTitle ∷ String → StoryBuilder Unit
setTitle s = liftFree <<< inj $ SetSTitle s unit

mkPlayer ∷ Array Oid → Rid → StoryBuilder Unit
mkPlayer os r = liftFree <<< inj $ MkPlayer os r unit

setMaxScore ∷ Int → StoryBuilder Unit
setMaxScore i = liftFree <<< inj $ SetMaxScore i unit

mkRoom ∷ Rid → RoomBuilder Unit → StoryBuilder Unit
mkRoom rid rb = liftFree <<< inj $ MkRoom rid rb unit

mkObject ∷ Oid → ObjectBuilder Unit → StoryBuilder Unit
mkObject oid ob = liftFree <<< inj $ MkObject oid ob unit

setSInit ∷ Action Unit → StoryBuilder Unit
setSInit atn = liftFree <<< inj $ SetSInit atn unit

mkState ∷ ∀ a . Typeable a ⇒ String → a → StoryBuilder (Sid a)
mkState l val = liftFree <<< inj $ MkState l (mkExists mkStateF)
  where mkStateF = MkStateF1 { val, toDyn: toDynamic, next: id }

setRTitle ∷ String → RoomBuilder Unit
setRTitle s = liftFree <<< inj $ SetRTitle s unit

setRDescr ∷ Action Unit → RoomBuilder Unit
setRDescr s = liftFree <<< inj $ SetRDescr s unit

setRExits ∷ ExitsBuilder Unit → RoomBuilder Unit
setRExits eb = liftFree <<< inj $ SetRExits eb unit

setRItems ∷ Array Oid → RoomBuilder Unit
setRItems oids = liftFree <<< inj $ SetRItems oids unit

setOTitle ∷ String → ObjectBuilder Unit
setOTitle s = liftFree <<< inj $ SetOTitle s unit

setODescr ∷ Action Unit → ObjectBuilder Unit
setODescr s = liftFree <<< inj $ SetODescr s unit

setOIsPlural ∷ Boolean → ObjectBuilder Unit
setOIsPlural b = liftFree <<< inj $ SetOIsPlural b unit

setONounType ∷ NounType → ObjectBuilder Unit
setONounType nt = liftFree <<< inj $ SetONounType nt unit

setOCanPickUp ∷ Boolean → ObjectBuilder Unit
setOCanPickUp b = liftFree <<< inj $ SetOCanPickUp b unit

class SetUse m where
  toSetUse ∷ m → Either (Action Unit) (UseAction Unit)

-- can't use type synonyms here!
-- instance setUse1 ∷ SetUse (Free ActionF Unit) where
instance setUse1 ∷ SetUse (FreeT (PrintLnF ⊕ AddItemF ⊕ TakeItemF ⊕ DestroyItemF ⊕ IncScoreF ⊕ SayF ⊕ PlayerHasF ⊕ RoomHasF ⊕ RoomOfF ⊕ CurrentRoomF ⊕ GetStateF ⊕ SetStateF) Identity Unit) where
  toSetUse = Left

-- instance setUse2 ∷ SetUse (Free UseActionF Unit) where
instance setUse2 ∷ SetUse (FreeT (WithF ⊕ GetStateF) Identity Unit) where
  toSetUse = Right

setOUse ∷ ∀ su. SetUse su ⇒ su → ObjectBuilder Unit
setOUse su = liftFree <<< inj $ SetOUse (toSetUse su) unit

talkO ∷ Action Unit → ObjectBuilder Unit
talkO a = liftFree <<< inj $ SetOTalk a unit


printLn ∷ String → Action Unit
printLn s = liftFree <<< inj $ PrintLn s unit

addItem ∷ Rid → Oid → Action Unit
addItem rid oid = liftFree <<< inj $ AddItem rid oid unit

takeItem ∷ Oid → Action Unit
takeItem oid = liftFree <<< inj $ TakeItem oid unit

destroyItem ∷ Oid → Action Unit
destroyItem oid = liftFree <<< inj $ DestroyItem oid unit

incScore ∷ Int → Action Unit
incScore i = liftFree <<< inj $ IncScore i unit

say ∷ String → Action Unit → Action Unit
say s a = liftFree <<< inj $ Say s a unit

playerHas ∷ Oid → Action Boolean
playerHas oid = liftFree <<< inj $ PlayerHas oid id

roomHas ∷ Rid → Oid → Action Boolean
roomHas rid oid = liftFree <<< inj $ RoomHas rid oid id

roomOf ∷ Oid → Action (Maybe Rid)
roomOf oid = liftFree <<< inj $ RoomOf oid id

currentRoom ∷ Action Rid
currentRoom = liftFree <<< inj $ CurrentRoom id

-- | Type class to enable reuse of getState for Action, UseAction, ExitsBuilder
class GetStateAction ma where
  getState2 ∷ ∀ a. Typeable a ⇒ Sid a → ma a

-- instance getStateAction ∷ GetStateAction (Free ActionF) where
instance getStateAction ∷ GetStateAction (FreeT (PrintLnF ⊕ AddItemF ⊕ TakeItemF ⊕ DestroyItemF ⊕ IncScoreF ⊕ SayF ⊕ PlayerHasF ⊕ RoomHasF ⊕ RoomOfF ⊕ CurrentRoomF ⊕ GetStateF ⊕ SetStateF) Identity) where
  getState2 sid = liftFree <<< inj $ GetState (mkExists state2)
    where state2 = GetStateF1 { sid, fromDyn: fromDynamic, next: id }

-- instance getStateAction1 ∷ GetStateAction (Free UseActionF) where
instance getStateAction1 ∷ GetStateAction (FreeT (WithF ⊕ GetStateF) Identity) where
  getState2 sid = liftFree <<< inj $ GetState (mkExists state2)
    where state2 = GetStateF1 { sid, fromDyn: fromDynamic, next: id }

-- instance getStateAction2 ∷ GetStateAction (Free ExitsBuilderF) where
instance getStateAction2 ∷ GetStateAction (FreeT (AddExitF ⊕ GetStateF) Identity) where
  getState2 sid = liftFree <<< inj $ GetState (mkExists state2)
    where state2 = GetStateF1 { sid, fromDyn: fromDynamic, next: id }

getState ∷ ∀ gsa a . GetStateAction gsa ⇒ Typeable a ⇒ Sid a → gsa a
getState sid = getState2 sid

setState ∷ ∀ a . Typeable a ⇒ Sid a → a → Action Unit
setState sid val = liftFree <<< inj $ SetState (mkExists state3)
  where state3 = SetStateF1 { sid, val, toDyn: toDynamic, next: unit }


with ∷ Oid → Action Unit → UseAction Unit
with oid atn = liftFree <<< inj $ With oid atn unit

class RidAction m where
  toAction ∷ m → Action (Maybe Rid)

-- instance ridAction1 ∷ RidAction (Free ActionF (Maybe Rid)) where
instance ridAction1 ∷ RidAction (FreeT (PrintLnF ⊕ AddItemF ⊕ TakeItemF ⊕ DestroyItemF ⊕ IncScoreF ⊕ SayF ⊕ PlayerHasF ⊕ RoomHasF ⊕ RoomOfF ⊕ CurrentRoomF ⊕ GetStateF ⊕ SetStateF) Identity (Maybe Rid)) where
  toAction = id

instance ridAction2 ∷ RidAction Rid where
  toAction = pure <<< Just

exit ∷ ∀ ra. RidAction ra ⇒ String → DirHint → ra → ExitsBuilder Unit
exit label dirHint ra = liftFree <<< inj $ AddExit {label, dirHint, rid: toAction ra} unit

-- | Type helper for use with exit, setOUse, to avoid ambiguous types
action ∷ ∀ r. Action r → Action r
action = id
