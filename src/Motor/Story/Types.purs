module Motor.Story.Types
  ( Action
  , ActionSyntax(..)
  , DirHint(..)
  , Exit
  , ExitsBuilder
  , ExitsBuilderF(..)
  , GetStateF(..)
  , MkStateF(..)
  , NounType(..)
  , Object
  , ObjectBuilder
  , ObjectBuilderF(..)
  , Oid(..)
  , Player
  , Rid(..)
  , Room
  , RoomBuilder
  , RoomBuilderF(..)
  , SetStateF(..)
  , Sid(..)
  , Story(..)
  , StoryBuilder
  , StoryBuilderF(..)
  , UseAction
  , UseActionF(..)
  ) where

import Prelude
import Control.Monad.Free (Free)
import Data.Dynamic (Dynamic)
import Data.Either (Either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)


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


-- TODO separate state from Story definition?
-- e.g. title, maxScore, init are immutable
-- Story available through Writer, StoryState through State
newtype Story = Story
  { title    ∷ Maybe String -- is required (currently lens with throw exception if not set...)
  , player   ∷ Player
  , rooms    ∷ M.Map Rid Room
  , objects  ∷ M.Map Oid Object
  , states   ∷ M.Map String Dynamic
  , score    ∷ Int
  , maxScore ∷ Maybe Int
  , say      ∷ Array (Tuple String (Action Unit))
  , init     ∷ Action Unit
  }
derive instance newtypeStory ∷ Newtype Story _

type Player =
  { inventory ∷ Array Oid
  , location  ∷ Maybe Rid -- is required (currently lens with throw exception if not set...)
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

mapMkStateExists ∷ ∀ a b. (a → b) → Exists (MkStateF a) → Exists (MkStateF b)
mapMkStateExists f exists =
  runExists (\(MkStateF {val, toDyn, next}) →
    mkExists $ MkStateF {val, toDyn, next: f <<< next}
  ) exists

mapGetStateExists ∷ ∀ a b. (a → b) → Exists (GetStateF a) → Exists (GetStateF b)
mapGetStateExists f exists =
  runExists (\(GetStateF {sid, fromDyn, next}) →
    mkExists $ GetStateF {sid, fromDyn, next: f <<< next}
  ) exists

mapSetStateExists ∷ ∀ a b. (a → b) → Exists (SetStateF a) → Exists (SetStateF b)
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
  , items        ∷ Array Oid -- or Set?
  }

data RoomBuilderF next
  = SetRTitle String              next
  | SetRDescr (Action Unit)       next
  | SetRExits (ExitsBuilder Unit) next
  | SetRItems (Array Oid)        next

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
  | MkPlayer    (Array Oid) Rid          next
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
