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

import Prelude (Unit, identity, pure, unit, ($), (<<<))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Functor.Coproduct.Inject (inj)
import Data.Functor.Product.Infix (type (⊕))
import Foreign (unsafeFromForeign, unsafeToForeign)
import Motor.Story.Types
import Control.Monad.Free.Trans (FreeT, liftFreeT)


setTitle ∷ String → StoryBuilder Unit
setTitle s = liftFreeT <<< inj $ SetSTitle s unit

mkPlayer ∷ Array Oid → Rid → StoryBuilder Unit
mkPlayer os r = liftFreeT <<< inj $ MkPlayer os r unit

setMaxScore ∷ Int → StoryBuilder Unit
setMaxScore i = liftFreeT <<< inj $ SetMaxScore i unit

mkRoom ∷ Rid → RoomBuilder Unit → StoryBuilder Unit
mkRoom rid rb = liftFreeT <<< inj $ MkRoom rid rb unit

mkObject ∷ Oid → ObjectBuilder Unit → StoryBuilder Unit
mkObject oid ob = liftFreeT <<< inj $ MkObject oid ob unit

setSInit ∷ Action Unit → StoryBuilder Unit
setSInit atn = liftFreeT <<< inj $ SetSInit atn unit

mkState ∷ ∀ a . String → a → StoryBuilder (Sid a)
mkState l val = liftFreeT <<< inj $ MkState l (mkExists mkStateF)
  where --mkStateF ∷ MkStateF (Sid a) a
        mkStateF = MkStateF1 { val, next: identity }

setRTitle ∷ String → RoomBuilder Unit
setRTitle s = liftFreeT <<< inj $ SetRTitle s unit

setRDescr ∷ Action Unit → RoomBuilder Unit
setRDescr s = liftFreeT <<< inj $ SetRDescr s unit

setRExits ∷ ExitsBuilder Unit → RoomBuilder Unit
setRExits eb = liftFreeT <<< inj $ SetRExits eb unit

setRItems ∷ Array Oid → RoomBuilder Unit
setRItems oids = liftFreeT <<< inj $ SetRItems oids unit

setOTitle ∷ String → ObjectBuilder Unit
setOTitle s = liftFreeT <<< inj $ SetOTitle s unit

setODescr ∷ Action Unit → ObjectBuilder Unit
setODescr s = liftFreeT <<< inj $ SetODescr s unit

setOIsPlural ∷ Boolean → ObjectBuilder Unit
setOIsPlural b = liftFreeT <<< inj $ SetOIsPlural b unit

setONounType ∷ NounType → ObjectBuilder Unit
setONounType nt = liftFreeT <<< inj $ SetONounType nt unit

setOCanPickUp ∷ Boolean → ObjectBuilder Unit
setOCanPickUp b = liftFreeT <<< inj $ SetOCanPickUp b unit

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
setOUse su = liftFreeT <<< inj $ SetOUse (toSetUse su) unit

talkO ∷ Action Unit → ObjectBuilder Unit
talkO a = liftFreeT <<< inj $ SetOTalk a unit


printLn ∷ String → Action Unit
printLn s = liftFreeT <<< inj $ PrintLn s unit

addItem ∷ Rid → Oid → Action Unit
addItem rid oid = liftFreeT <<< inj $ AddItem rid oid unit

takeItem ∷ Oid → Action Unit
takeItem oid = liftFreeT <<< inj $ TakeItem oid unit

destroyItem ∷ Oid → Action Unit
destroyItem oid = liftFreeT <<< inj $ DestroyItem oid unit

incScore ∷ Int → Action Unit
incScore i = liftFreeT <<< inj $ IncScore i unit

say ∷ String → Action Unit → Action Unit
say s a = liftFreeT <<< inj $ Say s a unit

playerHas ∷ Oid → Action Boolean
playerHas oid = liftFreeT <<< inj $ PlayerHas oid identity

roomHas ∷ Rid → Oid → Action Boolean
roomHas rid oid = liftFreeT <<< inj $ RoomHas rid oid identity

roomOf ∷ Oid → Action (Maybe Rid)
roomOf oid = liftFreeT <<< inj $ RoomOf oid identity

currentRoom ∷ Action Rid
currentRoom = liftFreeT <<< inj $ CurrentRoom identity

-- | Type class to enable reuse of getState for Action, UseAction, ExitsBuilder
class GetStateAction ma where
  getState2 ∷ ∀ a. Sid a → ma a

-- instance getStateAction ∷ GetStateAction (Free ActionF) where
instance getStateAction ∷ GetStateAction (FreeT (PrintLnF ⊕ AddItemF ⊕ TakeItemF ⊕ DestroyItemF ⊕ IncScoreF ⊕ SayF ⊕ PlayerHasF ⊕ RoomHasF ⊕ RoomOfF ⊕ CurrentRoomF ⊕ GetStateF ⊕ SetStateF) Identity) where
  getState2 sid = liftFreeT <<< inj $ GetState (mkExists state2)
    where state2 = GetStateF1 { sid, next: identity }

-- instance getStateAction1 ∷ GetStateAction (Free UseActionF) where
instance getStateAction1 ∷ GetStateAction (FreeT (WithF ⊕ GetStateF) Identity) where
  getState2 sid = liftFreeT <<< inj $ GetState (mkExists state2)
    where state2 = GetStateF1 { sid, next: identity }

-- instance getStateAction2 ∷ GetStateAction (Free ExitsBuilderF) where
instance getStateAction2 ∷ GetStateAction (FreeT (AddExitF ⊕ GetStateF) Identity) where
  getState2 sid = liftFreeT <<< inj $ GetState (mkExists state2)
    where state2 = GetStateF1 { sid, next: identity }

getState ∷ ∀ gsa a . GetStateAction gsa ⇒ Sid a → gsa a
getState sid = getState2 sid

setState ∷ ∀ a . Sid a → a → Action Unit
setState sid val = liftFreeT <<< inj $ SetState (mkExists state3)
  where state3 = SetStateF1 { sid, val, next: unit }


with ∷ Oid → Action Unit → UseAction Unit
with oid atn = liftFreeT <<< inj $ With oid atn unit

class RidAction m where
  toAction ∷ m → Action (Maybe Rid)

-- instance ridAction1 ∷ RidAction (Free ActionF (Maybe Rid)) where
instance ridAction1 ∷ RidAction (FreeT (PrintLnF ⊕ AddItemF ⊕ TakeItemF ⊕ DestroyItemF ⊕ IncScoreF ⊕ SayF ⊕ PlayerHasF ⊕ RoomHasF ⊕ RoomOfF ⊕ CurrentRoomF ⊕ GetStateF ⊕ SetStateF) Identity (Maybe Rid)) where
  toAction = identity

instance ridAction2 ∷ RidAction Rid where
  toAction = pure <<< Just

exit ∷ ∀ ra. RidAction ra ⇒ String → DirHint → ra → ExitsBuilder Unit
exit label dirHint ra = liftFreeT <<< inj $ AddExit {label, dirHint, rid: toAction ra} unit

-- | Type helper for use with exit, setOUse, to avoid ambiguous types
action ∷ ∀ r. Action r → Action r
action = identity
