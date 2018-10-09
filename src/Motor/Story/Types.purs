module Motor.Story.Types
  ( Action, ActionF, CoActionF
  , PrintLnF(..), AddItemF(..), TakeItemF(..), DestroyItemF(..), IncScoreF(..), SayF(..), PlayerHasF(..), RoomHasF(..), RoomOfF(..), CurrentRoomF(..)
  , CoPrintLnF(..), CoAddItemF(..), CoTakeItemF(..), CoDestroyItemF(..), CoIncScoreF(..), CoSayF(..), CoPlayerHasF(..), CoRoomHasF(..), CoRoomOfF(..), CoCurrentRoomF(..)
  , printLnPairing, addItemPairing, takeItemPairing, destroyItemPairing, incScorePairing, sayPairing, playerHasPairing, roomHasPairing, roomOfPairing, currentRoomPairing
  , DirHint(..)
  , Exit
  , ExitsBuilder, ExitsBuilderF, CoExitsBuilderF, AddExitF(..), CoAddExitF(..)
  , addExitPairing
  , MkStateF(..), MkStateF1(..), CoMkStateF(..), CoMkStateF1(..)
  , GetStateF(..), GetStateF1(..), CoGetStateF(..), CoGetStateF1(..)
  , SetStateF(..), SetStateF1(..), CoSetStateF(..), CoSetStateF1(..)
  , getStatePairing, setStatePairing
  , NounType(..)
  , Object
  , ObjectBuilder, ObjectBuilderF, CoObjectBuilderF
  , SetOTitleF(..), SetONounTypeF(..), SetOIsPluralF(..), SetODescrF(..), SetOCanPickUpF(..), SetOUseF(..), SetOTalkF(..)
  , CoSetOTitleF(..), CoSetONounTypeF(..), CoSetOIsPluralF(..), CoSetODescrF(..), CoSetOCanPickUpF(..), CoSetOUseF(..), CoSetOTalkF(..)
  , setOTitlePairing, setONounTypePairing, setOIsPluralPairing, setODescrPairing, setOCanPickUpPairing, setOUsePairing, setOTalkPairing
  , Oid(..)
  , Player
  , Rid(..)
  , Room
  , RoomBuilder, RoomBuilderF, CoRoomBuilderF
  , SetRTitleF(..), SetRDescrF(..), SetRExitsF(..), SetRItemsF(..)
  , CoSetRTitleF(..), CoSetRDescrF(..), CoSetRExitsF(..), CoSetRItemsF(..)
  , setRTitlePairing, setRDescrPairing, setRExitsPairing, setRItemsPairing
  , Sid(..)
  , Story(..)
  , StoryBuilder, StoryBuilderF, CoStoryBuilderF
  , SetSTitleF(..), MkPlayerF(..), MkObjectF(..), MkRoomF(..), SetSInitF(..), SetMaxScoreF(..)
  , CoSetSTitleF(..), CoMkPlayerF(..), CoMkObjectF(..), CoMkRoomF(..), CoSetSInitF(..), CoSetMaxScoreF(..)
  , setSTitlePairing, mkPlayerPairing, mkObjectPairing, mkRoomPairing, mkStatePairing, setSInitPairing, setMaxScorePairing
  , UseAction, UseActionF, CoUseActionF, WithF(..), CoWithF(..)
  , withPairing
  ) where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Data.Identity (Identity)
import Data.Either (Either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Pairing.PairEffect (tupleFuncPairing)
import Data.Functor.Product.Infix (type (⊕), type (⊗))
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)



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

-- | State id - phantom type `a` keeps track of the type of state being referenced
newtype Sid a = Sid String
derive instance ordSid ∷ Ord (Sid a)
derive instance eqSid  ∷ Eq (Sid a)
instance showSid ∷ Show (Sid a) where
  show (Sid s) = s


-- TODO separate state from Story definition?
-- e.g. title, maxScore, init are immutable
-- Story available through Writer, StoryState through State
newtype Story = Story
  { title    ∷ String
  , player   ∷ Player
  , rooms    ∷ M.Map Rid Room
  , objects  ∷ M.Map Oid Object
  -- | the state type is `Foreign` - a `Sid` key will be required when read/write
  -- | the state and indicates which type to coerce from
  , states   ∷ M.Map String Foreign
  , score    ∷ Int
  , maxScore ∷ Maybe Int
  , say      ∷ Array (Tuple String (Action Unit))
  , init     ∷ Action Unit
  }
derive instance newtypeStory ∷ Newtype Story _

type Player =
  { inventory ∷ Array Oid
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

--
type Room =
  { title        ∷ String
  , descr        ∷ Action Unit
  , exitsBuilder ∷ ExitsBuilder Unit
  , items        ∷ Array Oid -- or Set?
  }

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

--------------------------------------------------------------------------------

-- https://stackoverflow.com/questions/31592018/rank-2-types-in-data-constructors
-- https://stackoverflow.com/questions/36006483/how-to-use-type-constrains-with-exists
data MkStateF1 next a = MkStateF1 { val   ∷ a
                                  , next  ∷ Sid a → next
                                  }


data SetSTitleF   k = SetSTitle   String                        k
data MkPlayerF    k = MkPlayer    (Array Oid) Rid               k  -- TODO can we enforce set once-and-only-once?
data MkObjectF    k = MkObject    Oid (ObjectBuilder Unit)      k
data MkRoomF      k = MkRoom      Rid (RoomBuilder Unit)        k
data MkStateF     k = MkState     String (Exists (MkStateF1 k))
data SetSInitF    k = SetSInit    (Action Unit)                 k
data SetMaxScoreF k = SetMaxScore Int                           k
derive instance setSTitleFunctor   ∷ Functor SetSTitleF
derive instance mkPlayerFunctor    ∷ Functor MkPlayerF
derive instance mkObjectFunctor    ∷ Functor MkObjectF
derive instance mkRoomFunctor      ∷ Functor MkRoomF
derive instance setSInitFFunctor   ∷ Functor SetSInitF
derive instance setMaxScoreFunctor ∷ Functor SetMaxScoreF
-- -- we can't derive Functor with existential quantification
instance mkStateFunctor ∷ Functor MkStateF where
  map f (MkState l exists) = MkState l (mapMkStateExists f exists)
    where
      mapMkStateExists ∷ ∀ a b. (a → b) → Exists (MkStateF1 a) → Exists (MkStateF1 b)
      mapMkStateExists f exists =
        runExists (\(MkStateF1 {val, next}) →
          mkExists $ MkStateF1 {val, next: f <<< next}
        ) exists



type StoryBuilderF
  = SetSTitleF
  ⊕ MkPlayerF
  ⊕ MkObjectF
  ⊕ MkRoomF
  ⊕ MkStateF
  ⊕ SetSInitF
  ⊕ SetMaxScoreF

type StoryBuilder = FreeT StoryBuilderF Identity



data CoMkStateF1 next a = CoMkStateF1 { next ∷ String → a → Tuple (Sid a) next
                                      }

data CoSetSTitleF   k = CoSetSTitle   (String                   → k)
data CoMkPlayerF    k = CoMkPlayer    ((Array Oid) → Rid        → k)
data CoMkObjectF    k = CoMkObject    (Oid → ObjectBuilder Unit → k)
data CoMkRoomF      k = CoMkRoom      (Rid → RoomBuilder Unit   → k)
data CoMkStateF     k = CoMkState     (Exists (CoMkStateF1 k))
data CoSetSInitF    k = CoSetSInit    ((Action Unit)            → k)
data CoSetMaxScoreF k = CoSetMaxScore (Int                      → k)

derive instance coSetSTitleFunctor   ∷ Functor CoSetSTitleF
derive instance coMkPlayerFunctor    ∷ Functor CoMkPlayerF
derive instance coMkObjectFunctor    ∷ Functor CoMkObjectF
derive instance coMkRoomFunctor      ∷ Functor CoMkRoomF
derive instance coSetSInitFFunctor   ∷ Functor CoSetSInitF
derive instance coSetMaxScoreFunctor ∷ Functor CoSetMaxScoreF

-- -- we can't derive Functor with existential quantification
instance coMkStateFunctor ∷ Functor CoMkStateF where
  map f (CoMkState exists) = CoMkState (mapCoMkStateExists f exists)
    where
      mapCoMkStateExists ∷ ∀ a b. (a → b) → Exists (CoMkStateF1 a) → Exists (CoMkStateF1 b)
      mapCoMkStateExists f exists =
        runExists (\(CoMkStateF1 { next }) →
          let next2 l a = Tuple s (f k2)
                          where Tuple s k2 = next l a
          in mkExists $ CoMkStateF1 { next: next2 }
        ) exists


type CoStoryBuilderF
  = CoSetSTitleF
  ⊗ CoMkPlayerF
  ⊗ CoMkObjectF
  ⊗ CoMkRoomF
  ⊗ CoMkStateF
  ⊗ CoSetSInitF
  ⊗ CoSetMaxScoreF



setSTitlePairing ∷ CoSetSTitleF ⋈ SetSTitleF
setSTitlePairing f (CoSetSTitle g) (SetSTitle t k) =
  f (g t) k

mkPlayerPairing ∷ CoMkPlayerF ⋈ MkPlayerF
mkPlayerPairing f (CoMkPlayer g) (MkPlayer os r k) =
    f (g os r) k

mkObjectPairing ∷ CoMkObjectF ⋈ MkObjectF
mkObjectPairing f (CoMkObject g) (MkObject o ob k) =
    f (g o ob) k

mkRoomPairing ∷ CoMkRoomF ⋈ MkRoomF
mkRoomPairing f (CoMkRoom g) (MkRoom r rb k) =
    f (g r rb) k

mkStatePairing ∷ CoMkStateF ⋈ MkStateF
mkStatePairing f (CoMkState existsG) (MkState t k) =
    runExists (\(CoMkStateF1 { next } ) →
      let g = next
      in runExists (\(MkStateF1 { val, next }) →
        let h = next
            pair2 ∷ ∀ a b r c. (a → b → r) → Tuple (Sid c) a → (Sid c → b) → r
            pair2 f tuple func = tupleFuncPairing f tuple func
            g2 = unsafeCoerce (g t (unsafeCoerce val))
        in pair2 f g2 h
      ) k
    ) existsG

setSInitPairing ∷ CoSetSInitF ⋈ SetSInitF
setSInitPairing f (CoSetSInit g) (SetSInit a k) =
    f (g a) k

setMaxScorePairing ∷ CoSetMaxScoreF ⋈ SetMaxScoreF
setMaxScorePairing f (CoSetMaxScore g) (SetMaxScore i k) =
    f (g i) k


--------------------------------------------------------------------------------

data SetRTitleF k = SetRTitle String              k
data SetRDescrF k = SetRDescr (Action Unit)       k
data SetRExitsF k = SetRExits (ExitsBuilder Unit) k
data SetRItemsF k = SetRItems (Array Oid)         k
derive instance setRTitleFunctor ∷ Functor SetRTitleF
derive instance setRDescrFunctor ∷ Functor SetRDescrF
derive instance setRExitsFunctor ∷ Functor SetRExitsF
derive instance setRItemsFunctor ∷ Functor SetRItemsF

type RoomBuilderF
  = SetRTitleF
  ⊕ SetRDescrF
  ⊕ SetRExitsF
  ⊕ SetRItemsF
type RoomBuilder = FreeT RoomBuilderF Identity

data CoSetRTitleF k = CoSetRTitle (String            → k)
data CoSetRDescrF k = CoSetRDescr (Action Unit       → k)
data CoSetRExitsF k = CoSetRExits (ExitsBuilder Unit → k)
data CoSetRItemsF k = CoSetRItems ((Array Oid)       → k)
derive instance coSetRTitleFunctor ∷ Functor CoSetRTitleF
derive instance coSetRDescrFunctor ∷ Functor CoSetRDescrF
derive instance coSetRExitsFunctor ∷ Functor CoSetRExitsF
derive instance coSetRItemsFunctor ∷ Functor CoSetRItemsF

type CoRoomBuilderF
  = CoSetRTitleF
  ⊗ CoSetRDescrF
  ⊗ CoSetRExitsF
  ⊗ CoSetRItemsF


setRTitlePairing ∷ CoSetRTitleF ⋈ SetRTitleF
setRTitlePairing f (CoSetRTitle g) (SetRTitle t k) =
    f (g t) k

setRDescrPairing ∷ CoSetRDescrF ⋈ SetRDescrF
setRDescrPairing f (CoSetRDescr g) (SetRDescr a k) =
    f (g a) k

setRExitsPairing ∷ CoSetRExitsF ⋈ SetRExitsF
setRExitsPairing f (CoSetRExits g) (SetRExits ea k) =
    f (g ea) k

setRItemsPairing ∷ CoSetRItemsF ⋈ SetRItemsF
setRItemsPairing f (CoSetRItems g) (SetRItems os k) =
    f (g os) k

--------------------------------------------------------------------------------

data SetOTitleF     k = SetOTitle     String                                  k
data SetONounTypeF  k = SetONounType  NounType                                k
data SetOIsPluralF  k = SetOIsPlural  Boolean                                 k
data SetODescrF     k = SetODescr     (Action Unit)                           k
data SetOCanPickUpF k = SetOCanPickUp Boolean                                 k
data SetOUseF       k = SetOUse       (Either (Action Unit) (UseAction Unit)) k
data SetOTalkF      k = SetOTalk      (Action Unit)                           k
derive instance setOTitleFunctor     ∷ Functor SetOTitleF
derive instance setONounTypeFunctor  ∷ Functor SetONounTypeF
derive instance setOIsPluralFunctor  ∷ Functor SetOIsPluralF
derive instance setODescrFunctor     ∷ Functor SetODescrF
derive instance setOCanPickUpFunctor ∷ Functor SetOCanPickUpF
derive instance setOUseFunctor       ∷ Functor SetOUseF
derive instance stOTalkFunctor       ∷ Functor SetOTalkF

type ObjectBuilderF
  = SetOTitleF
  ⊕ SetONounTypeF
  ⊕ SetOIsPluralF
  ⊕ SetODescrF
  ⊕ SetOCanPickUpF
  ⊕ SetOUseF
  ⊕ SetOTalkF
type ObjectBuilder = FreeT ObjectBuilderF Identity

data CoSetOTitleF     k = CoSetOTitle     (String                                → k)
data CoSetONounTypeF  k = CoSetONounType  (NounType                              → k)
data CoSetOIsPluralF  k = CoSetOIsPlural  (Boolean                               → k)
data CoSetODescrF     k = CoSetODescr     (Action Unit                           → k)
data CoSetOCanPickUpF k = CoSetOCanPickUp (Boolean                               → k)
data CoSetOUseF       k = CoSetOUse       (Either (Action Unit) (UseAction Unit) → k)
data CoSetOTalkF      k = CoSetOTalk      (Action Unit                           → k)
derive instance coSetOTitleFunctor     ∷ Functor CoSetOTitleF
derive instance coSetONounTypeFunctor  ∷ Functor CoSetONounTypeF
derive instance coSetOIsPluralFunctor  ∷ Functor CoSetOIsPluralF
derive instance coSetODescrFunctor     ∷ Functor CoSetODescrF
derive instance coSetOCanPickUpFunctor ∷ Functor CoSetOCanPickUpF
derive instance coSetOUseFunctor       ∷ Functor CoSetOUseF
derive instance coSetOTalkFunctor      ∷ Functor CoSetOTalkF

type CoObjectBuilderF
  = CoSetOTitleF
  ⊗ CoSetONounTypeF
  ⊗ CoSetOIsPluralF
  ⊗ CoSetODescrF
  ⊗ CoSetOCanPickUpF
  ⊗ CoSetOUseF
  ⊗ CoSetOTalkF


setOTitlePairing ∷ CoSetOTitleF ⋈ SetOTitleF
setOTitlePairing f (CoSetOTitle g) (SetOTitle t k) =
    f (g t) k

setONounTypePairing ∷ CoSetONounTypeF ⋈ SetONounTypeF
setONounTypePairing f (CoSetONounType g) (SetONounType nt k) =
    f (g nt) k

setOIsPluralPairing ∷ CoSetOIsPluralF ⋈ SetOIsPluralF
setOIsPluralPairing f (CoSetOIsPlural g) (SetOIsPlural b k) =
    f (g b) k

setODescrPairing ∷ CoSetODescrF ⋈ SetODescrF
setODescrPairing f (CoSetODescr g) (SetODescr a k) =
    f (g a) k

setOCanPickUpPairing ∷ CoSetOCanPickUpF ⋈ SetOCanPickUpF
setOCanPickUpPairing f (CoSetOCanPickUp g) (SetOCanPickUp b k) =
    f (g b) k

setOUsePairing ∷ CoSetOUseF ⋈ SetOUseF
setOUsePairing f (CoSetOUse g) (SetOUse ua k) =
    f (g ua) k

setOTalkPairing ∷ CoSetOTalkF ⋈ SetOTalkF
setOTalkPairing f (CoSetOTalk g) (SetOTalk a k) =
   f (g a) k

--------------------------------------------------------------------------------

data GetStateF1 next a = GetStateF1 { sid     ∷ Sid a
                                    , next    ∷ a → next
                                    }
data SetStateF1 next a = SetStateF1 { sid   ∷ Sid a
                                    , val   ∷ a
                                    , next  ∷ next
                                    }


data GetStateF k = GetState (Exists (GetStateF1 k))
data SetStateF k = SetState (Exists (SetStateF1 k))

instance getStateFunctor ∷ Functor GetStateF where
  map f (GetState exists) = GetState (mapGetStateExists f exists)
    where
      mapGetStateExists ∷ ∀ a b. (a → b) → Exists (GetStateF1 a) → Exists (GetStateF1 b)
      mapGetStateExists f exists =
        runExists (\(GetStateF1 {sid, next}) →
          mkExists $ GetStateF1 {sid, next: f <<< next}
        ) exists

instance setStateFunctor ∷ Functor SetStateF where
  map f (SetState exists) = SetState (mapSetStateExists f exists)
    where
      mapSetStateExists ∷ ∀ a b. (a → b) → Exists (SetStateF1 a) → Exists (SetStateF1 b)
      mapSetStateExists f exists =
        runExists (\(SetStateF1 {sid, val, next}) →
          mkExists $ SetStateF1 {sid, val, next: f next}
        ) exists

-- TODO switch from record to function (newtype?) -- or even drop type, and define as Exists `(Sid a → ... )`
data CoGetStateF1 next a = CoGetStateF1 { next  ∷ Sid a → Tuple a next }
data CoSetStateF1 next a = CoSetStateF1 { next  ∷ Sid a → a → next }

data CoGetStateF        k = CoGetState (Exists (CoGetStateF1 k))
data CoSetStateF        k = CoSetState (Exists (CoSetStateF1 k))

instance coGetStateFunctor ∷ Functor CoGetStateF where
  map f (CoGetState exists) = CoGetState (mapCoGetStateExists f exists)
    where
      mapCoGetStateExists ∷ ∀ a b. (a → b) → Exists (CoGetStateF1 a) → Exists (CoGetStateF1 b)
      mapCoGetStateExists f exists =
        runExists (\(CoGetStateF1 {next}) →
          let next2 s = Tuple a (f k2)
                        where Tuple a k2 = next s
          in mkExists $ CoGetStateF1 {next: next2}
        ) exists

instance coSetStateFunctor ∷ Functor CoSetStateF where
  map f (CoSetState exists) = --CoSetState (\s → let (v, k2) = k s in (v, f k2))
                              CoSetState (mapCoSetStateExists f exists)
    where
      mapCoSetStateExists ∷ ∀ a b. (a → b) → Exists (CoSetStateF1 a) → Exists (CoSetStateF1 b)
      mapCoSetStateExists f exists =
        runExists (\(CoSetStateF1 { next }) →
          let next2 s a = f (next s a)
          in mkExists $ CoSetStateF1 { next: next2 }
        ) exists


getStatePairing ∷ CoGetStateF ⋈ GetStateF
getStatePairing f (CoGetState existsG) (GetState k) =
    runExists (\(CoGetStateF1 { next } ) →
      let g = next
      in runExists (\(GetStateF1 { sid, next: h }) →
           let pair2 ∷ ∀ a b r c. (a → b → r) → Tuple c a → (c → b) → r
               pair2 f tuple func = tupleFuncPairing f tuple func
               g2 = unsafeCoerce (g (unsafeCoerce sid))
           in pair2 f g2 h
         ) k
    ) existsG

setStatePairing ∷ CoSetStateF ⋈ SetStateF
setStatePairing f (CoSetState existsG) (SetState k) =
    runExists (\(CoSetStateF1 { next } ) →
      let g = next
      in runExists (\(SetStateF1 { sid, val, next: h }) →
           let g2 = unsafeCoerce (g (unsafeCoerce sid) (unsafeCoerce val))
           in f g2 h
         ) k
    ) existsG



--------------------------------------------------------------------------------


data PrintLnF     k = PrintLn     String                        k
data AddItemF     k = AddItem     Rid Oid                       k
data TakeItemF    k = TakeItem    Oid                           k
data DestroyItemF k = DestroyItem Oid                           k
data IncScoreF    k = IncScore    Int                           k
data SayF         k = Say         String (Action Unit)          k
data PlayerHasF   k = PlayerHas   Oid              (Boolean   → k)
data RoomHasF     k = RoomHas     Rid Oid          (Boolean   → k)
data RoomOfF      k = RoomOf      Oid              (Maybe Rid → k) -- assumes oid only in one room...
data CurrentRoomF k = CurrentRoom                  (Rid       → k)
derive instance printLnFunctor     ∷ Functor PrintLnF
derive instance addItemFunctor     ∷ Functor AddItemF
derive instance takeItemFunctor    ∷ Functor TakeItemF
derive instance destroyItemFunctor ∷ Functor DestroyItemF
derive instance incScoreFunctor    ∷ Functor IncScoreF
derive instance sayFunctor         ∷ Functor SayF
derive instance playerHasFunctor   ∷ Functor PlayerHasF
derive instance roomHasFunctor     ∷ Functor RoomHasF
derive instance roomOfFunctor      ∷ Functor RoomOfF
derive instance currentRoomFunctor ∷ Functor CurrentRoomF

type ActionF
  = PrintLnF
  ⊕ AddItemF
  ⊕ TakeItemF
  ⊕ DestroyItemF
  ⊕ IncScoreF
  ⊕ SayF
  ⊕ PlayerHasF
  ⊕ RoomHasF
  ⊕ RoomOfF
  ⊕ CurrentRoomF
  ⊕ GetStateF ⊕ SetStateF

type Action = FreeT ActionF Identity

data CoPrintLnF     k = CoPrintLn     (String               →                   k)
data CoAddItemF     k = CoAddItem     (Rid → Oid            →                   k)
data CoTakeItemF    k = CoTakeItem    (Oid                  →                   k)
data CoDestroyItemF k = CoDestroyItem (Oid                  →                   k)
data CoIncScoreF    k = CoIncScore    (Int                  →                   k)
data CoSayF         k = CoSay         (String → Action Unit →                   k)
data CoPlayerHasF   k = CoPlayerHas   (Oid                  → Tuple Boolean     k)
data CoRoomHasF     k = CoRoomHas     (Rid → Oid            → Tuple Boolean     k)
data CoRoomOfF      k = CoRoomOf      (Oid                  → Tuple (Maybe Rid) k)
data CoCurrentRoomF k = CoCurrentRoom (                       Tuple Rid         k)
derive instance coPrintLnFunctor     ∷ Functor CoPrintLnF
derive instance coAddItemFunctor     ∷ Functor CoAddItemF
derive instance coTakeItemFunctor    ∷ Functor CoTakeItemF
derive instance coDestroyItemFunctor ∷ Functor CoDestroyItemF
derive instance coIncScoreFunctor    ∷ Functor CoIncScoreF
derive instance coSayFunctor         ∷ Functor CoSayF
derive instance coPlayerHasFunctor   ∷ Functor CoPlayerHasF
derive instance coRoomHasFunctor     ∷ Functor CoRoomHasF
derive instance coRoomOfFunctor      ∷ Functor CoRoomOfF
derive instance coCurrentRoomFunctor ∷ Functor CoCurrentRoomF

type CoActionF
  = CoPrintLnF
  ⊗ CoAddItemF
  ⊗ CoTakeItemF
  ⊗ CoDestroyItemF
  ⊗ CoIncScoreF
  ⊗ CoSayF
  ⊗ CoPlayerHasF
  ⊗ CoRoomHasF
  ⊗ CoRoomOfF
  ⊗ CoCurrentRoomF
  ⊗ CoGetStateF
  ⊗ CoSetStateF


printLnPairing ∷ CoPrintLnF ⋈ PrintLnF
printLnPairing f (CoPrintLn g) (PrintLn t k) =
    f (g t) k

addItemPairing ∷ CoAddItemF ⋈ AddItemF
addItemPairing f (CoAddItem g) (AddItem r o k) =
    f (g r o) k

takeItemPairing ∷ CoTakeItemF ⋈ TakeItemF
takeItemPairing f (CoTakeItem g) (TakeItem o k) =
    f (g o) k

destroyItemPairing ∷ CoDestroyItemF ⋈ DestroyItemF
destroyItemPairing f (CoDestroyItem g) (DestroyItem o k) =
    f (g o) k

incScorePairing ∷ CoIncScoreF ⋈ IncScoreF
incScorePairing f (CoIncScore g) (IncScore i k) =
    f (g i) k

sayPairing ∷ CoSayF ⋈ SayF
sayPairing f (CoSay g) (Say s a k) =
    f (g s a) k

playerHasPairing ∷ CoPlayerHasF ⋈ PlayerHasF
playerHasPairing f (CoPlayerHas g) (PlayerHas o k) =
    tupleFuncPairing f (g o) k

roomHasPairing ∷ CoRoomHasF ⋈ RoomHasF
roomHasPairing f (CoRoomHas g) (RoomHas r o k) =
    tupleFuncPairing f (g r o) k

roomOfPairing ∷ CoRoomOfF ⋈ RoomOfF
roomOfPairing f (CoRoomOf g) (RoomOf o k) =
    tupleFuncPairing f (g o) k

currentRoomPairing ∷ CoCurrentRoomF ⋈ CurrentRoomF
currentRoomPairing f (CoCurrentRoom g) (CurrentRoom k) =
    tupleFuncPairing f g k


--------------------------------------------------------------------------------

data AddExitF k = AddExit Exit k
derive instance addExitFunctor ∷ Functor AddExitF

type ExitsBuilderF
  = AddExitF
  ⊕ GetStateF

type ExitsBuilder = FreeT ExitsBuilderF Identity

data CoAddExitF k = CoAddExit (Exit → k)
derive instance coAddExitFunctor ∷ Functor CoAddExitF

type CoExitsBuilderF
  = CoAddExitF
  ⊗ CoGetStateF

addExitPairing ∷ CoAddExitF ⋈ AddExitF
addExitPairing f (CoAddExit g) (AddExit e k) =
    f (g e) k


--------------------------------------------------------------------------------

data WithF k = With Oid (Action Unit) k
derive instance withFunctor ∷ Functor WithF

type UseActionF
  = WithF
  ⊕ GetStateF

type UseAction = FreeT UseActionF Identity

data CoWithF k = CoWith (Oid → Action Unit → k)
derive instance coWithFunctor ∷ Functor CoWithF

type CoUseActionF
  = CoWithF
  ⊗ CoGetStateF

withPairing ∷ CoWithF ⋈ WithF
withPairing f (CoWith g) (With o a k) =
    f (g o a) k

--------------------------------------------------------------------------------
