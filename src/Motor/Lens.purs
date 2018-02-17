module Motor.Lens
  ( sTitle
  , sPlayer
  , sTitle
  , sPlayer
  , sRooms
  , sObjects
  , sStates
  , sScore
  , sMaxScore
  , sSay
  , sInit
  , pInventory
  , pLocation
  , eLabel
  , eDirHint
  , eRid
  , rTitle
  , rDescr
  , rExitsBuilder
  , rItems
  , oTitle
  , oNounType
  , oIsPlural
  , oDescr
  , oCanPickUp
  , oUse
  , oTalk
  -- reexports
  , module Data.Lens.At
  , module Data.Lens
) where

import Prelude (Unit)
import Data.Dynamic (Dynamic)
import Data.Either (Either)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Motor.Story (Action, DirHint, Exit, ExitsBuilder, NounType, Object, Oid, Player, Rid, Room, Story, UseAction)
import Data.Lens (Lens', lens, (^.), (.~), (%~), _Just, to)
import Data.Lens.At (at)


-- * Auto derived lenses?
-- E.g. https://github.com/paf31/purescript-derive-lenses
--   purs compile src/**/*.purs bower_components/purescript-*/src/**/*.purs --dump-corefn
--   && pulp run -- -i output/Motor.Story/corefn.json -o test-files/Output.purs
--
--   pulp run should be running purescript-derive-lenses (`-m Main.purs` is ambiguous)
--   however, the parameters are not being passed through to the module, but to purs
--   (https://github.com/purescript-contrib/pulp/issues/309)



-- manual lens creation..
sTitle ∷ Lens' Story String
sTitle = lens (_.title) (\s title -> s { title = title })

sPlayer ∷ Lens' Story Player
sPlayer = lens (_.player) (\s player -> s { player = player })

sRooms ∷ Lens' Story (M.Map Rid Room)
sRooms = lens (_.rooms) (\s rooms -> s { rooms = rooms })

sObjects ∷ Lens' Story (M.Map Oid Object)
sObjects = lens (_.objects) (\s objects -> s { objects = objects })

sStates ∷ Lens' Story (M.Map String Dynamic)
sStates = lens (_.states) (\s states -> s { states = states })

sScore ∷ Lens' Story Int
sScore = lens (_.score) (\s score -> s { score = score })

sMaxScore ∷ Lens' Story (Maybe Int)
sMaxScore = lens (_.maxScore) (\s maxScore -> s { maxScore = maxScore })

sSay ∷ Lens' Story (Array (Tuple String (Action Unit)))
sSay = lens (_.say) (\s say -> s { say = say })

sInit ∷ Lens' Story (Action Unit)
sInit = lens (_.init) (\s init -> s { init = init })

pInventory ∷ Lens' Player (L.List Oid)
pInventory = lens (_.inventory) (\p inventory -> p { inventory = inventory })

pLocation ∷ Lens' Player Rid
pLocation = lens (_.location) (\p location -> p { location = location })

eLabel ∷ Lens' Exit String
eLabel = lens (_.label) (\e label -> e { label = label })

eDirHint ∷ Lens' Exit DirHint
eDirHint = lens (_.dirHint) (\e dirHint -> e { dirHint = dirHint })

eRid ∷ Lens' Exit (Action (Maybe Rid))
eRid = lens (_.rid) (\e rid -> e { rid = rid })

rTitle ∷ Lens' Room String
rTitle = lens (_.title) (\r title -> r { title = title })

rDescr ∷ Lens' Room (Action Unit)
rDescr = lens (_.descr) (\r descr -> r { descr = descr })

rExitsBuilder ∷ Lens' Room (ExitsBuilder Unit)
rExitsBuilder = lens (_.exitsBuilder) (\r exitsBuilder -> r { exitsBuilder = exitsBuilder })

rItems ∷ Lens' Room (L.List Oid)
rItems = lens (_.items) (\r items -> r { items = items })


oTitle ∷ Lens' Object String
oTitle = lens (_.title) (\o title -> o { title = title })

oNounType ∷ Lens' Object NounType
oNounType = lens (_.nounType) (\o nounType -> o { nounType = nounType })

oIsPlural ∷ Lens' Object Boolean
oIsPlural = lens (_.isPlural) (\o isPlural -> o { isPlural = isPlural })

oDescr ∷ Lens' Object (Action Unit)
oDescr = lens (_.descr) (\o descr -> o { descr = descr })

oCanPickUp ∷ Lens' Object Boolean
oCanPickUp = lens (_.canPickUp) (\o canPickUp -> o { canPickUp = canPickUp })

oUse ∷ Lens' Object (Either (Action Unit) (UseAction Unit))
oUse = lens (_.use) (\o use -> o { use = use })

oTalk ∷ Lens' Object (Maybe (Action Unit))
oTalk = lens (_.talk) (\o talk -> o { talk = talk })