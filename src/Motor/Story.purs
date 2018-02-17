module Motor.Story where

import Prelude
import Control.Monad.Free (Free, runFreeM, liftF)
import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (for_)
import Data.Map as M
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Dynamic (Dynamic, toDynamic, fromDynamic)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Typeable (class Typeable)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)


newtype Rid = Rid String
derive instance ordRid ∷ Ord Rid
derive instance eqRid  ∷ Eq Rid
instance showRid ∷ Show Rid where
  show (Rid s) = s

newtype Oid = Oid String
derive instance ordOid ∷ Ord Oid
derive instance eqOid  ∷ Eq Oid
instance showOid ∷ Show Oid where
  show (Oid s) = s

newtype Sid a = Sid String
derive instance ordSid ∷ Ord (Sid a)
derive instance eqSid  ∷ Eq (Sid a)
instance showSid ∷ Show (Sid a) where
  show (Sid s) = s


type Story =
  { title    ∷ String
  , player   ∷ Player
  , rooms    ∷ M.Map Rid Room
  , objects  ∷ M.Map Oid Object
  , states   ∷ M.Map String Dynamic
  , score    ∷ Int
  , maxScore ∷ Maybe Int
  , say      ∷ Array (Tuple String (Action Unit))
  , init     ∷ Action Unit
  }

type Player =
  { inventory ∷ L.List Oid
  , location  ∷ Rid
  }

data DirHint = N | W | E | S | U
derive instance eqDirHint ∷ Eq DirHint
derive instance ordDirHint ∷ Ord DirHint

type Exit =
  { label   ∷ String
  , dirHint ∷ DirHint
  , rid     ∷ Action (Maybe Rid)
  }

-- instance eqExit ∷ Eq Exit where
--   eq e1 e2 = eq e1.label e2.label
-- instance ordExit ∷ Ord Exit where
--   compare e1 e2 = compare e1.dirHint e2.dirHint

data ExitsBuilderF next
  = AddExit Exit next
  | GetState2    (Exists (GetStateF next))

mapMkStateExists :: ∀ a b. (a → b) → Exists (MkStateF a) → Exists (MkStateF b)
mapMkStateExists f exists =
  runExists (\(MkStateF {val, toDyn, next}) →
    mkExists $ MkStateF {val, toDyn, next: f <<< next}
  ) exists

mapGetStateExists :: ∀ a b. (a → b) → Exists (GetStateF a) → Exists (GetStateF b)
mapGetStateExists f exists =
  runExists (\(GetStateF {sid, fromDyn, next}) →
    mkExists $ GetStateF {sid, fromDyn, next: f <<< next}
  ) exists

mapSetStateExists :: ∀ a b. (a → b) → Exists (SetStateF a) → Exists (SetStateF b)
mapSetStateExists f exists =
  runExists (\(SetStateF {sid, val, next, toDyn}) →
    mkExists $ SetStateF {sid, val, toDyn, next: f next}
  ) exists


instance functorExitsBuilderF ∷ Functor ExitsBuilderF where
  map f (AddExit e   next) = AddExit e (f next)
  map f (GetState2 exists) = GetState2 (mapGetStateExists f exists)

type ExitsBuilder = Free ExitsBuilderF

type Room =
  { title        ∷ String
  , descr        ∷ Action Unit
  , exitsBuilder ∷ ExitsBuilder Unit
  , items        ∷ L.List Oid
  }

data RoomBuilderF next
  = SetRTitle String              next
  | SetRDescr (Action Unit)       next
  | SetRExits (ExitsBuilder Unit) next
  | SetRItems (L.List Oid)        next

derive instance functorRoomBuilderF ∷ Functor RoomBuilderF

type RoomBuilder = Free RoomBuilderF


data NounType = Proper
              | Quantitive -- "some"
              | Particular -- "the" , "a/an"

type Object =
  { title     ∷ String
  , nounType  ∷ NounType
  , isPlural  ∷ Boolean
  , descr     ∷ Action Unit
  , canPickUp ∷ Boolean
  , use       ∷ Either (Action Unit) (UseAction Unit)
  , talk      ∷ Maybe (Action Unit)
  }

data UseActionF next
  = With Oid (Action Unit) next
  | GetState1              (Exists (GetStateF next))

instance functionUseActionF ∷ Functor UseActionF where
  map f (With o a    next) = With o a (f next)
  map f (GetState1 exists) = GetState1 (mapGetStateExists f exists)

type UseAction = Free UseActionF


data ObjectBuilderF next
  = SetOTitle     String                                  next
  | SetONounType  NounType                                next
  | SetOIsPlural  Boolean                                 next
  | SetODescr     (Action Unit)                           next
  | SetOCanPickUp Boolean                                 next
  | SetOUse       (Either (Action Unit) (UseAction Unit)) next
  | SetOTalk      (Action Unit)                           next

derive instance functorObjectBuilderF ∷ Functor ObjectBuilderF

type ObjectBuilder = Free ObjectBuilderF



-- https://stackoverflow.com/questions/31592018/rank-2-types-in-data-constructors
-- https://stackoverflow.com/questions/36006483/how-to-use-type-constrains-with-exists
data MkStateF next a = MkStateF { val   ∷ a
                                , toDyn ∷ a → Dynamic
                                , next  ∷ Sid a → next
                                }

data StoryBuilderF next
  = SetSTitle   String                   next
  | MkPlayer    (L.List Oid) Rid         next
  | SetMaxScore Int                      next
  | MkRoom      Rid (RoomBuilder Unit)   next
  | MkObject    Oid (ObjectBuilder Unit) next
  | SetSInit    (Action Unit)            next
  | MkState     String                   (Exists (MkStateF next))

-- derive instance functorStoryBuilderF ∷ Functor StoryBuilderF
-- can't derive functor with Exists

instance functorStoryBuilderF ∷ Functor StoryBuilderF where
  map f (SetSTitle s      next  ) = SetSTitle s      (f next)
  map f (MkPlayer  os r   next  ) = MkPlayer  os r   (f next)
  map f (SetMaxScore i    next  ) = SetMaxScore i    (f next)
  map f (MkRoom    rid rb next  ) = MkRoom    rid rb (f next)
  map f (MkObject  oid ob next  ) = MkObject  oid ob (f next)
  map f (SetSInit  a      next  ) = SetSInit  a      (f next)
  map f (MkState   s      exists) = MkState s (mapMkStateExists f exists)



type StoryBuilder = Free StoryBuilderF

setTitle ∷ String → StoryBuilder Unit
setTitle s = liftF $ SetSTitle s unit

mkPlayer ∷ Array Oid → Rid → StoryBuilder Unit
mkPlayer os r = liftF $ MkPlayer (L.fromFoldable os) r unit

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
setRItems oids = liftF $ SetRItems (L.fromFoldable oids) unit -- array is a better dsl for creation - what's best internal model? set?

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

data GetStateF next a = GetStateF { sid     ∷ Sid a
                                  , fromDyn ∷ Dynamic → Maybe a
                                  , next    ∷ a → next
                                  }

data SetStateF next a = SetStateF { sid   ∷ Sid a
                                  , val   ∷ a
                                  , toDyn ∷ a → Dynamic
                                  , next  ∷ next
                                  }


data ActionSyntax next
  = PrintLn String                next
  | AddItem Rid Oid               next
  | TakeItem Oid                  next
  | DestroyItem Oid               next
  | IncScore Int                  next
  | Say String (Action Unit)      next
  | PlayerHas Oid   (Boolean   → next)
  | RoomHas Rid Oid (Boolean   → next)
  | RoomOf Oid      (Maybe Rid → next)
  | CurrentRoom     (Rid       → next)
  | GetState        (Exists (GetStateF next))
  | SetState        (Exists (SetStateF next))


instance functionActionSyntax ∷ Functor ActionSyntax where
  map f (PrintLn s     next) = PrintLn s     (f next)
  map f (AddItem r o   next) = AddItem r o   (f next)
  map f (TakeItem o    next) = TakeItem o    (f next)
  map f (DestroyItem o next) = DestroyItem o (f next)
  map f (IncScore i    next) = IncScore i    (f next)
  map f (Say s a       next) = Say s a       (f next)
  map f (PlayerHas o   next) = PlayerHas o   (f <<< next)
  map f (RoomHas r o   next) = RoomHas r o   (f <<< next)
  map f (RoomOf o      next) = RoomOf o      (f <<< next)
  map f (CurrentRoom   next) = CurrentRoom   (f <<< next)
  map f (GetState    exists) = GetState (mapGetStateExists f exists)
  map f (SetState    exists) = SetState (mapSetStateExists f exists)

type Action = Free ActionSyntax

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

-- Type class to enable reuse of getState for Action, UseAction, ExitsBuilder
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
