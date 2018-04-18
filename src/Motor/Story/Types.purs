module Motor.Story.Types
  ( Action, ActionF, CoActionF
  , PrintLnF(..), AddItemF(..), TakeItemF(..), DestroyItemF(..), IncScoreF(..), SayF(..), PlayerHasF(..), RoomHasF(..), RoomOfF(..), CurrentRoomF(..)
  , CoPrintLnF(..), CoAddItemF(..), CoTakeItemF(..), CoDestroyItemF(..), CoIncScoreF(..), CoSayF(..), CoPlayerHasF(..), CoRoomHasF(..), CoRoomOfF(..), CoCurrentRoomF(..)
  , DirHint(..)
  , Exit
  , ExitsBuilder, ExitsBuilderF, CoExitsBuilderF, AddExitF(..), CoAddExitF(..)
  , MkStateF(..), MkStateF1(..), CoMkStateF(..), CoMkStateF1(..)
  , GetStateF(..), GetStateF1(..), CoGetStateF(..), CoGetStateF1(..)
  , SetStateF(..), SetStateF1(..), CoSetStateF(..), CoSetStateF1(..)
  , NounType(..)
  , Object
  , ObjectBuilder, ObjectBuilderF, CoObjectBuilderF
  , SetOTitleF(..), SetONounTypeF(..), SetOIsPluralF(..), SetODescrF(..), SetOCanPickUpF(..), SetOUseF(..), SetOTalkF(..)
  , CoSetOTitleF(..), CoSetONounTypeF(..), CoSetOIsPluralF(..), CoSetODescrF(..), CoSetOCanPickUpF(..), CoSetOUseF(..), CoSetOTalkF(..)
  , Oid(..)
  , Player
  , Rid(..)
  , Room
  , RoomBuilder, RoomBuilderF, CoRoomBuilderF
  , SetRTitleF(..), SetRDescrF(..), SetRExitsF(..), SetRItemsF(..)
  , CoSetRTitleF(..), CoSetRDescrF(..), CoSetRExitsF(..), CoSetRItemsF(..)
  , Sid(..)
  , Story(..)
  , StoryBuilder, StoryBuilderF, CoStoryBuilderF
  , SetSTitleF(..), MkPlayerF(..), MkObjectF(..), MkRoomF(..), SetSInitF(..), SetMaxScoreF(..)
  , CoSetSTitleF(..), CoMkPlayerF(..), CoMkObjectF(..), CoMkRoomF(..), CoSetSInitF(..), CoSetMaxScoreF(..)
  , UseAction, UseActionF, CoUseActionF, WithF(..), CoWithF(..)
  ) where

import Prelude

import Proact.Monad.Trans.Free (FreeT)
import Data.Identity (Identity)
import Data.Dynamic (Dynamic)
import Data.Either (Either)
import Data.Exists (Exists, mkExists, runExists)
import Coproduct (type (⊕), type (⊗))
import Pairing (class Pairing, pair)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
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
                                  , toDyn ∷ a → Dynamic
                                  , next  ∷ Sid a → next
                                  }


data SetSTitleF   k = SetSTitle   String                               k
data MkPlayerF    k = MkPlayer    (Array Oid) Rid                      k  -- TODO can we enforce set once-and-only-once?
data MkObjectF    k = MkObject    Oid (ObjectBuilder Unit)             k
data MkRoomF      k = MkRoom      Rid (RoomBuilder Unit)               k
data MkStateF     k = MkState     String (Exists (MkStateF1 k))
data SetSInitF    k = SetSInit    (Action Unit)                        k
data SetMaxScoreF k = SetMaxScore Int                                  k
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
        runExists (\(MkStateF1 {val, toDyn, next}) →
          mkExists $ MkStateF1 {val, toDyn, next: f <<< next}
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



data CoMkStateF1 next a = CoMkStateF1 { next ∷ String → a → (a → Dynamic) → Tuple (Sid a) next
                                      }

data CoSetSTitleF   k = CoSetSTitle   (                        String                   →         k )
data CoMkPlayerF    k = CoMkPlayer    (                        (Array Oid) → Rid        →         k )
data CoMkObjectF    k = CoMkObject    (                        Oid → ObjectBuilder Unit →         k )
data CoMkRoomF      k = CoMkRoom      (                        Rid → RoomBuilder Unit   →         k )
data CoMkStateF     k = CoMkState     (Exists (CoMkStateF1 k))
data CoSetSInitF    k = CoSetSInit    (                        (Action Unit)            →         k )
data CoSetMaxScoreF k = CoSetMaxScore (                        Int                      →         k )

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
          let next2 l a toDyn = Tuple s (f k2)
                            where Tuple s k2 = next l a toDyn
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



instance sTitlePairing ∷ Pairing CoSetSTitleF SetSTitleF where
  pair f (CoSetSTitle g) (SetSTitle t k) =
    f (g t) k

instance mkPlayerPairing ∷ Pairing CoMkPlayerF MkPlayerF where
  pair f (CoMkPlayer g) (MkPlayer os r k) =
    f (g os r) k

instance mkObjectPairing ∷ Pairing CoMkObjectF MkObjectF where
  pair f (CoMkObject g) (MkObject o ob k) =
    f (g o ob) k

instance mkRoomPairing∷ Pairing CoMkRoomF MkRoomF where
  pair f (CoMkRoom g) (MkRoom r rb k) =
    f (g r rb) k

instance mkStatePairing ∷ Pairing CoMkStateF MkStateF where
  pair f (CoMkState existsG) (MkState t k) =
    runExists (\(CoMkStateF1 { next } ) →
      let g = next
      in runExists (\(MkStateF1 { val, toDyn, next }) →
        let h = next
            -- how to match the two existentials?
            pair2 ∷ ∀ a b r c. (a → b → r) → Tuple (Sid c) a → (Sid c → b) → r
            pair2 f tuple func = pair f tuple func
            g2 = unsafeCoerce (g t (unsafeCoerce val) (unsafeCoerce toDyn))
        in pair2 f g2 h
      ) k
    ) existsG

instance setSInitPairing ∷ Pairing CoSetSInitF SetSInitF where
  pair f (CoSetSInit g) (SetSInit a k) =
    f (g a) k

instance setMaxScorePairing ∷ Pairing CoSetMaxScoreF SetMaxScoreF where
  pair f (CoSetMaxScore g) (SetMaxScore i k) =
    f (g i) k


--------------------------------------------------------------------------------

data SetRTitleF k = SetRTitle String              k
data SetRDescrF k = SetRDescr (Action Unit)       k
data SetRExitsF k = SetRExits (ExitsBuilder Unit) k
data SetRItemsF k = SetRItems (Array Oid)         k
derive instance setRTitleFunctor   ∷ Functor SetRTitleF
derive instance setRDescrFunctor   ∷ Functor SetRDescrF
derive instance setRExitsFunctor   ∷ Functor SetRExitsF
derive instance setRItemsFunctor   ∷ Functor SetRItemsF

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
derive instance coSetRTitleFunctor   ∷ Functor CoSetRTitleF
derive instance coSetRDescrFunctor   ∷ Functor CoSetRDescrF
derive instance coSetRExitsFunctor   ∷ Functor CoSetRExitsF
derive instance coSetRItemsFunctor   ∷ Functor CoSetRItemsF

type CoRoomBuilderF
  = CoSetRTitleF
  ⊗ CoSetRDescrF
  ⊗ CoSetRExitsF
  ⊗ CoSetRItemsF


instance setRTitlePairing ∷ Pairing CoSetRTitleF SetRTitleF where
  pair f (CoSetRTitle g) (SetRTitle t k) =
    f (g t) k

instance setRDescrPairing ∷ Pairing CoSetRDescrF SetRDescrF where
  pair f (CoSetRDescr g) (SetRDescr a k) =
    f (g a) k

instance setRExitsPairing ∷ Pairing CoSetRExitsF SetRExitsF where
  pair f (CoSetRExits g) (SetRExits ea k) =
    f (g ea) k

instance setRItemsPairing ∷ Pairing CoSetRItemsF SetRItemsF where
  pair f (CoSetRItems g) (SetRItems os k) =
    f (g os) k

--------------------------------------------------------------------------------

data SetOTitleF     k = SetOTitle     String           k
data SetONounTypeF  k = SetONounType  NounType         k
data SetOIsPluralF  k = SetOIsPlural  Boolean          k
data SetODescrF     k = SetODescr     (Action Unit)    k
data SetOCanPickUpF k = SetOCanPickUp Boolean          k
data SetOUseF       k = SetOUse       (Either (Action Unit) (UseAction Unit)) k
data SetOTalkF      k = SetOTalk      (Action Unit)    k
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

data CoSetOTitleF     k = CoSetOTitle     (String          → k)
data CoSetONounTypeF  k = CoSetONounType  (NounType        → k)
data CoSetOIsPluralF  k = CoSetOIsPlural  (Boolean         → k)
data CoSetODescrF     k = CoSetODescr     (Action Unit     → k)
data CoSetOCanPickUpF k = CoSetOCanPickUp (Boolean         → k)
data CoSetOUseF       k = CoSetOUse       (Either (Action Unit) (UseAction Unit) → k)
data CoSetOTalkF      k = CoSetOTalk      (Action Unit     → k)
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


instance setOTitlePairing ∷ Pairing CoSetOTitleF SetOTitleF where
  pair f (CoSetOTitle g) (SetOTitle t k) =
    f (g t) k

instance setONounTypePairing ∷ Pairing CoSetONounTypeF SetONounTypeF where
  pair f (CoSetONounType g) (SetONounType nt k) =
    f (g nt) k

instance setIsPluralPairing ∷ Pairing CoSetOIsPluralF SetOIsPluralF where
  pair f (CoSetOIsPlural g) (SetOIsPlural b k) =
    f (g b) k

instance setODescrPairing ∷ Pairing CoSetODescrF SetODescrF where
  pair f (CoSetODescr g) (SetODescr a k) =
    f (g a) k

instance setOCanPickUpPairing ∷ Pairing CoSetOCanPickUpF SetOCanPickUpF where
  pair f (CoSetOCanPickUp g) (SetOCanPickUp b k) =
    f (g b) k

instance setOUsePairing ∷ Pairing CoSetOUseF SetOUseF where
  pair f (CoSetOUse g) (SetOUse ua k) =
    f (g ua) k

instance setOTalkPairing ∷ Pairing CoSetOTalkF SetOTalkF where
  pair f (CoSetOTalk g) (SetOTalk a k) =
   f (g a) k

--------------------------------------------------------------------------------

data GetStateF1 next a = GetStateF1 { sid     ∷ Sid a
                                    , fromDyn ∷ Dynamic → Maybe a
                                    , next    ∷ a → next
                                    }
data SetStateF1 next a = SetStateF1 { sid   ∷ Sid a
                                    , val   ∷ a
                                    , toDyn ∷ a → Dynamic
                                    , next  ∷ next
                                    }


data GetStateF k = GetState (Exists (GetStateF1 k))
data SetStateF k = SetState (Exists (SetStateF1 k))

instance getStateFunctor ∷ Functor GetStateF where
  map f (GetState exists) = GetState (mapGetStateExists f exists)
    where
      mapGetStateExists ∷ ∀ a b. (a → b) → Exists (GetStateF1 a) → Exists (GetStateF1 b)
      mapGetStateExists f exists =
        runExists (\(GetStateF1 {sid, fromDyn, next}) →
          mkExists $ GetStateF1 {sid, fromDyn, next: f <<< next}
        ) exists

instance setStateFunctor ∷ Functor SetStateF where
  map f (SetState exists) = SetState (mapSetStateExists f exists)
    where
      mapSetStateExists ∷ ∀ a b. (a → b) → Exists (SetStateF1 a) → Exists (SetStateF1 b)
      mapSetStateExists f exists =
        runExists (\(SetStateF1 {sid, val, next, toDyn}) →
          mkExists $ SetStateF1 {sid, val, toDyn, next: f next}
        ) exists

data CoGetStateF1 next a = CoGetStateF1 { next  ∷ Sid a → (Dynamic → Maybe a) → Tuple a next }
data CoSetStateF1 next a = CoSetStateF1 { next  ∷ Sid a → a → (a → Dynamic) → next }

data CoGetStateF        k = CoGetState (Exists (CoGetStateF1 k))
data CoSetStateF        k = CoSetState (Exists (CoSetStateF1 k))

instance coGetStateFunctor ∷ Functor CoGetStateF where
  map f (CoGetState exists) = CoGetState (mapCoGetStateExists f exists)
    where
      mapCoGetStateExists ∷ ∀ a b. (a → b) → Exists (CoGetStateF1 a) → Exists (CoGetStateF1 b)
      mapCoGetStateExists f exists =
        runExists (\(CoGetStateF1 {next}) →
          let next2 s fromDyn = Tuple a (f k2)
                            where Tuple a k2 = next s fromDyn
          in mkExists $ CoGetStateF1 {next: next2}
        ) exists

instance coSetStateFunctor ∷ Functor CoSetStateF where
  map f (CoSetState exists) = --CoSetState (\s → let (v, k2) = k s in (v, f k2))
                              CoSetState (mapCoSetStateExists f exists)
    where
      mapCoSetStateExists ∷ ∀ a b. (a → b) → Exists (CoSetStateF1 a) → Exists (CoSetStateF1 b)
      mapCoSetStateExists f exists =
        runExists (\(CoSetStateF1 { next }) →
          let next2 s a toDyn = f (next s a toDyn)
          in mkExists $ CoSetStateF1 { next: next2}
        ) exists


instance getStatePairing ∷ Pairing CoGetStateF GetStateF where
  pair f (CoGetState existsG) (GetState k) =
    runExists (\(CoGetStateF1 { next } ) →
      let g = next
      in runExists (\(GetStateF1 { sid, fromDyn, next }) →
           let h = next
               pair2 ∷ ∀ a b r c. (a → b → r) → Tuple c a → (c → b) → r
               pair2 f tuple func = pair f tuple func
               g2 = unsafeCoerce (g (unsafeCoerce sid) (unsafeCoerce fromDyn))
           in pair2 f g2 h
         ) k
    ) existsG

instance setStatePairing ∷ Pairing CoSetStateF SetStateF where
  pair f (CoSetState existsG) (SetState k) =
    runExists (\(CoSetStateF1 { next } ) →
      let g = next
      in runExists (\(SetStateF1 { sid, val, toDyn, next }) →
           let h = next
               g2 = unsafeCoerce (g (unsafeCoerce sid) (unsafeCoerce val) (unsafeCoerce toDyn))
           in f g2 h
         ) k
    ) existsG



--------------------------------------------------------------------------------


data PrintLnF     k = PrintLn     String                         k
data AddItemF     k = AddItem     Rid Oid                        k
data TakeItemF    k = TakeItem    Oid                            k
data DestroyItemF k = DestroyItem Oid                            k
data IncScoreF    k = IncScore    Int                            k
data SayF         k = Say         String (Action Unit)           k
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


instance printLnPairing ∷ Pairing CoPrintLnF PrintLnF where
  pair f (CoPrintLn g) (PrintLn t k) =
    f (g t) k

instance addItemPairing ∷ Pairing CoAddItemF AddItemF where
  pair f (CoAddItem g) (AddItem r o k) =
    f (g r o) k

instance takeItemPairing ∷ Pairing CoTakeItemF TakeItemF where
  pair f (CoTakeItem g) (TakeItem o k) =
    f (g o) k

instance destroyItemPairing ∷ Pairing CoDestroyItemF DestroyItemF where
  pair f (CoDestroyItem g) (DestroyItem o k) =
    f (g o) k

instance incScorePairing ∷ Pairing CoIncScoreF IncScoreF where
  pair f (CoIncScore g) (IncScore i k) =
    f (g i) k

instance sayPairing ∷ Pairing CoSayF SayF where
  pair f (CoSay g) (Say s a k) =
    f (g s a) k

instance playerHasPairing ∷ Pairing CoPlayerHasF PlayerHasF where
  pair f (CoPlayerHas g) (PlayerHas o k) =
    pair f (g o) k

instance roomHasPairing ∷ Pairing CoRoomHasF RoomHasF where
  pair f (CoRoomHas g) (RoomHas r o k) =
    pair f (g r o) k

instance roomOfPairing ∷ Pairing CoRoomOfF RoomOfF where
  pair f (CoRoomOf g) (RoomOf o k) =
    pair f (g o) k

instance currentRoomPairing ∷ Pairing CoCurrentRoomF CurrentRoomF where
  pair f (CoCurrentRoom g) (CurrentRoom k) =
    pair f g k


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

instance addExitPairing ∷ Pairing CoAddExitF AddExitF where
  pair f (CoAddExit g) (AddExit e k) =
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

instance withPairing ∷ Pairing CoWithF WithF where
  pair f (CoWith g) (With o a k) =
    f (g o a) k

--------------------------------------------------------------------------------
