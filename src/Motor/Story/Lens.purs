module Motor.Story.Lens
  ( getTitle
  , setTitle
  , sPlayer
  , sRooms
  , sObjects
  , sStates
  , sScore
  , sMaxScore
  , sSay
  , sInit
  , pInventory
  , getLocation
  , setLocation
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
  -- composites
  , sInventory
  , sLocation
  -- reexports
  , module Data.Lens.At
  , module Data.Lens
) where

import Prelude (Unit, (<<<))
import Data.Dynamic (Dynamic)
import Data.Either (Either)
import Data.Lens
import Data.Lens.At (at)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Newtype as NT
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafeCrashWith)

import Motor.Story.Types (Action, DirHint, Exit, ExitsBuilder, NounType, Object, Oid, Player, Rid, Room, Story(..), UseAction)


-- * Auto derived lenses?
-- E.g. https://github.com/paf31/purescript-derive-lenses
--   purs compile src/**/*.purs bower_components/purescript-*/src/**/*.purs --dump-corefn
--   && pulp run -- -i output/Motor.Types/corefn.json -o test-files/Output.purs
--
--   pulp run should be running purescript-derive-lenses (`-m Main.purs` is ambiguous)
--   however, the parameters are not being passed through to the module, but to purs
--   (https://github.com/purescript-contrib/pulp/issues/309)



-- manual lens creation..

getTitle ∷ Getter' Story String
getTitle = to \s →
            -- not using maybe since eagerly evaluates the default value
            case (unwrap s).title of
              Just a  → a
              Nothing → unsafeCrashWith "title - not set"

-- | Asymmetric setter from getter since getter does not expose laziness.
--   the setter must, since it will read the current value to transform it.
setTitle ∷ Setter' Story (Maybe String)
setTitle = \f story → let s = unwrap story
                       in wrap (s { title = f (s.title) })

sPlayer ∷ Lens' Story Player
sPlayer = lens (\s → (unwrap s).player) (\s player → NT.over Story (_ { player = player }) s)

sRooms ∷ Lens' Story (M.Map Rid Room)
sRooms = lens (\s → (unwrap s).rooms) (\s rooms → NT.over Story (_ { rooms = rooms }) s)

sObjects ∷ Lens' Story (M.Map Oid Object)
sObjects = lens (\s → (unwrap s).objects) (\s objects → NT.over Story (_ { objects = objects }) s)

sStates ∷ Lens' Story (M.Map String Dynamic)
sStates = lens (\s → (unwrap s).states) (\s states → NT.over Story (_ { states = states }) s)

sScore ∷ Lens' Story Int
sScore = lens (\s → (unwrap s).score) (\s score → NT.over Story (_ { score = score }) s)

sMaxScore ∷ Lens' Story (Maybe Int)
sMaxScore = lens (\s → (unwrap s).maxScore) (\s maxScore → NT.over Story (_ { maxScore = maxScore }) s)

sSay ∷ Lens' Story (Array (Tuple String (Action Unit)))
sSay = lens (\s → (unwrap s).say) (\s say → NT.over Story (_ { say = say }) s)

sInit ∷ Lens' Story (Action Unit)
sInit = lens (\s → (unwrap s).init) (\s init → NT.over Story (_ { init = init }) s)

pInventory ∷ Lens' Player (Array Oid)
pInventory = lens (_.inventory) (\p inventory → p { inventory = inventory })

getLocation∷ Getter' Player Rid
getLocation = to \player →
                -- not using maybe since eagerly evaluates the default value
                case player.location of
                  Just a  → a
                  Nothing → unsafeCrashWith "location - not set"

-- | Asymmetric setter from getter since getter does not expose laziness.
--   the setter must, since it will read the current value to transform it.
setLocation ∷ Setter' Player (Maybe Rid)
setLocation = \f player → player { location = f player.location }

eLabel ∷ Lens' Exit String
eLabel = lens (_.label) (\e label → e { label = label })

eDirHint ∷ Lens' Exit DirHint
eDirHint = lens (_.dirHint) (\e dirHint → e { dirHint = dirHint })

eRid ∷ Lens' Exit (Action (Maybe Rid))
eRid = lens (_.rid) (\e rid → e { rid = rid })

rTitle ∷ Lens' Room String
rTitle = lens (_.title) (\r title → r { title = title })

rDescr ∷ Lens' Room (Action Unit)
rDescr = lens (_.descr) (\r descr → r { descr = descr })

rExitsBuilder ∷ Lens' Room (ExitsBuilder Unit)
rExitsBuilder = lens (_.exitsBuilder) (\r exitsBuilder → r { exitsBuilder = exitsBuilder })

rItems ∷ Lens' Room (Array Oid)
rItems = lens (_.items) (\r items → r { items = items })


oTitle ∷ Lens' Object String
oTitle = lens (_.title) (\o title → o { title = title })

oNounType ∷ Lens' Object NounType
oNounType = lens (_.nounType) (\o nounType → o { nounType = nounType })

oIsPlural ∷ Lens' Object Boolean
oIsPlural = lens (_.isPlural) (\o isPlural → o { isPlural = isPlural })

oDescr ∷ Lens' Object (Action Unit)
oDescr = lens (_.descr) (\o descr → o { descr = descr })

oCanPickUp ∷ Lens' Object Boolean
oCanPickUp = lens (_.canPickUp) (\o canPickUp → o { canPickUp = canPickUp })

oUse ∷ Lens' Object (Either (Action Unit) (UseAction Unit))
oUse = lens (_.use) (\o use → o { use = use })

oTalk ∷ Lens' Object (Maybe (Action Unit))
oTalk = lens (_.talk) (\o talk → o { talk = talk })


-- useful composites

sInventory ∷ Lens' Story (Array Oid)
sInventory = sPlayer <<< pInventory

sLocation ∷ Getter' Story Rid
sLocation = sPlayer <<< getLocation
