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
import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe(..))
import Data.Dynamic (toDynamic, fromDynamic)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Typeable (class Typeable)
import Motor.Story.Types
  (Action
  , ActionSyntax(..)
  , DirHint
  , ExitsBuilder
  , ExitsBuilderF(..)
  , GetStateF(..)
  , MkStateF(..)
  , NounType
  , ObjectBuilder
  , ObjectBuilderF(..)
  , Oid
  , Rid
  , RoomBuilder
  , RoomBuilderF(..)
  , SetStateF(..)
  , Sid
  , StoryBuilder
  , StoryBuilderF(..)
  , UseAction
  , UseActionF(..)
  )


setTitle ∷ String → StoryBuilder Unit
setTitle s = liftF $ SetSTitle s unit

mkPlayer ∷ Array Oid → Rid → StoryBuilder Unit
mkPlayer os r = liftF $ MkPlayer os r unit

setMaxScore ∷ Int → StoryBuilder Unit
setMaxScore i = liftF $ SetMaxScore i unit

mkRoom ∷ Rid → RoomBuilder Unit → StoryBuilder Unit
mkRoom rid rb = liftF $ MkRoom rid rb unit

mkObject ∷ Oid → ObjectBuilder Unit → StoryBuilder Unit
mkObject oid ob = liftF $ MkObject oid ob unit

setSInit ∷ Action Unit → StoryBuilder Unit
setSInit atn = liftF $ SetSInit atn unit

mkState ∷ ∀ a . Typeable a ⇒ String → a → StoryBuilder (Sid a)
mkState l val = liftF $ MkState l (mkExists mkStateF)
  where mkStateF ∷ MkStateF (Sid a) a
        mkStateF = MkStateF { val, toDyn: toDynamic, next: id }


setRTitle ∷ String → RoomBuilder Unit
setRTitle s = liftF $ SetRTitle s unit

setRDescr ∷ Action Unit → RoomBuilder Unit
setRDescr s = liftF $ SetRDescr s unit

setRExits ∷ ExitsBuilder Unit → RoomBuilder Unit
setRExits eb = liftF $ SetRExits eb unit

setRItems ∷ Array Oid → RoomBuilder Unit
setRItems oids = liftF $ SetRItems oids unit

setOTitle ∷ String → ObjectBuilder Unit
setOTitle s = liftF $ SetOTitle s unit

setODescr ∷ Action Unit → ObjectBuilder Unit
setODescr s = liftF $ SetODescr s unit

setOIsPlural ∷ Boolean → ObjectBuilder Unit
setOIsPlural b = liftF $ SetOIsPlural b unit

setONounType ∷ NounType → ObjectBuilder Unit
setONounType nt = liftF $ SetONounType nt unit

setOCanPickUp ∷ Boolean → ObjectBuilder Unit
setOCanPickUp b = liftF $ SetOCanPickUp b unit

class SetUse m where
  toSetUse ∷ m → Either (Action Unit) (UseAction Unit)

instance setUse1 ∷ SetUse (Free ActionSyntax Unit) where
  toSetUse = Left

instance setUse2 ∷ SetUse (Free UseActionF Unit) where
  toSetUse = Right

setOUse ∷ ∀ su. SetUse su ⇒ su → ObjectBuilder Unit
setOUse su = liftF $ SetOUse (toSetUse su) unit

talkO ∷ Action Unit → ObjectBuilder Unit
talkO a = liftF $ SetOTalk a unit


printLn ∷ String → Action Unit
printLn s = liftF $ PrintLn s unit

addItem ∷ Rid → Oid → Action Unit
addItem rid oid = liftF $ AddItem rid oid unit

takeItem ∷ Oid → Action Unit
takeItem oid = liftF $ TakeItem oid unit

destroyItem ∷ Oid → Action Unit
destroyItem oid = liftF $ DestroyItem oid unit

incScore ∷ Int → Action Unit
incScore i = liftF $ IncScore i unit

say ∷ String → Action Unit → Action Unit
say s a = liftF $ Say s a unit

playerHas ∷ Oid → Action Boolean
playerHas oid = liftF $ PlayerHas oid id

roomHas ∷ Rid → Oid → Action Boolean
roomHas rid oid = liftF $ RoomHas rid oid id

roomOf ∷ Oid → Action (Maybe Rid)
roomOf oid = liftF $ RoomOf oid id

currentRoom ∷ Action Rid
currentRoom = liftF $ CurrentRoom id

-- | Type class to enable reuse of getState for Action, UseAction, ExitsBuilder
class GetStateAction ma where
  getState2 ∷ ∀ a. Typeable a ⇒ Sid a → ma a

instance getStateAction ∷ GetStateAction (Free ActionSyntax) where
  getState2 sid = liftF $ GetState (mkExists state2)
    where state2 = GetStateF { sid, fromDyn: fromDynamic, next: id }

instance getStateAction1 ∷ GetStateAction (Free UseActionF) where
  getState2 sid = liftF $ GetState1 (mkExists state2)
    where state2 = GetStateF { sid, fromDyn: fromDynamic, next: id }

instance getStateAction2 ∷ GetStateAction (Free ExitsBuilderF) where
  getState2 sid = liftF $ GetState2 (mkExists state2)
    where state2 = GetStateF { sid, fromDyn: fromDynamic, next: id }

getState ∷ ∀ gsa a . GetStateAction gsa ⇒ Typeable a ⇒ Sid a → gsa a
getState sid = getState2 sid

setState ∷ ∀ a . Typeable a ⇒ Sid a → a → Action Unit
setState sid val = liftF $ SetState (mkExists state3)
  where state3 ∷ SetStateF Unit a
        state3 = SetStateF { sid, val, toDyn: toDynamic, next: unit }


with ∷ Oid → Action Unit → UseAction Unit
with oid atn = liftF $ With oid atn unit

class RidAction m where
  toAction ∷ m → Action (Maybe Rid)

instance ridAction1 ∷ RidAction (Free ActionSyntax (Maybe Rid)) where
  toAction = id

instance ridAction2 ∷ RidAction Rid where
  toAction = pure <<< Just

exit ∷ ∀ ra. RidAction ra ⇒ String → DirHint → ra → ExitsBuilder Unit
exit label dirHint ra = liftF $ AddExit {label, dirHint, rid: toAction ra} unit

-- | Type helper for use with exit, setOUse, to avoid ambiguous types
action ∷ ∀ r. Action r → Action r
action = id
