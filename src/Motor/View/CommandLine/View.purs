module Motor.View.CommandLine.View
  ( commandLineView
  ) where

import Prelude
import Control.Monad.State.Trans (get, modify_)
import Control.Monad.Reader.Trans (ask)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Node.ReadLine (createConsoleInterface, noCompletion, question) as RL
import Motor.Interpreter.StoryInterpreter (buildStory)
import Motor.Interpreter.ActionInterpreter (runAction)
import Motor.Story.Types (Action, Object, Oid, Rid, Room, StoryBuilder)
import Motor.Story.Lens (sInit, sInventory, sSay, sTitle, (.~), (^.))
import Motor.Util (currentRoom, goto, listItems, listExits, takeItemS, the, toObject, useWith, useItself)
import Motor.View.CommandLine.Types (SS, runSS)


-- | Runs the app as a command-line app
commandLineView ∷ StoryBuilder Unit → Effect Unit
commandLineView sb = do
  let story = buildStory sb
  EC.log "----------"
  EC.log (story ^.sTitle)
  EC.log "----------"

  interface ← RL.createConsoleInterface RL.noCompletion

  let loop1 ∷ SS Unit
      loop1 = do
                initStory -- this assumes we're starting from the beginning. If we replay history here, then this will be handled.
                displayRoom
                loop

  runSS story interface loop1



initStory ∷ SS Unit
initStory = do
  story ← get
  initTxt ← runAction $ story ^. sInit
  putStrLns initTxt

putStrLns ∷ Array String → SS Unit
putStrLns = putStrLn <<< intercalate "\n"

putStrLn ∷ String → SS Unit
putStrLn = liftEffect <<< EC.log

loop ∷ SS Unit
loop = do
  room  ← currentRoom
  exits ← listExits room
  displayInRoom room
  initOptions >>= selectOption
  loop



data Option
  = ShowInventory (Array Oid)
  | Take          (Array Oid)
  | Use           (Array Oid)
  | Examine       (Array Oid)
  | TalkTo String (Action Unit)
  | Say    String (Action Unit)
  | InventoryO String Oid
  | ExamineO   String Oid
  | TakeO      String Oid
  | UseO       String Oid
  | UseWith    String Oid String Oid
  | Go String (Action (Maybe Rid))


instance showOption ∷ Show Option where
  show _ = "Option"


withObj ∷ ∀ a. (Tuple Oid Object → SS a) → Oid → SS a
withObj f oid = do
  obj ← toObject oid
  f $ Tuple oid obj


initOptions ∷ SS (Array Option)
initOptions = do
  story ← get
  let inventory      = story ^. sInventory
  room               ← currentRoom
  itemsToPickUp      ← A.filterA (\oid → (toObject oid >>= pure <<< _.canPickUp)) room.items
  let itemsToUse     = {-inventory <> -} {-L.filter (isLeft <<< _.use <<< toObject state.story)-} room.items
      itemsToExamine = room.items
  toTalkTo           ← map A.concat $
                        flip traverse room.items $
                          withObj \(Tuple _ obj) → pure $ case obj.talk of
                                                             Just atn → [TalkTo obj.title atn]
                                                             Nothing  → []
  exits              ← listExits room
  pure $  (if A.null inventory      then [] else [ShowInventory inventory     ])
       <> (if A.null itemsToPickUp  then [] else [Take          itemsToPickUp ])
       <> (if A.null itemsToUse     then [] else [Use           itemsToUse    ])
       <> (if A.null itemsToExamine then [] else [Examine       itemsToExamine])
       <> toTalkTo
       <> (if A.null exits         then [] else map (\e → Go e.label e.rid) exits)


displayRoom ∷ SS Unit
displayRoom = do
  room ← currentRoom
  putStrLn $ "\n" <> room.title <> "\n"
  descr ← runAction room.descr
  putStrLns descr

displayInRoom ∷ Room → SS Unit
displayInRoom room = do
  mTxt ← listItems room
  case mTxt of
    Nothing  → pure unit
    Just txt → putStrLn $ "\n" <> txt
  putStrLn ""
  exits ← listExits room
  putStrLn $ "The following exits are visible: " <> intercalate ", " (map _.label exits) <> "."

question
  :: String
  -> SS String
question q = do
    interface ← ask
    liftAff $ makeAff $ go interface
  where
    go interface handler = RL.question q (handler <<< Right) interface $> nonCanceler

selectOption ∷ Array Option → SS Unit
selectOption options = do
    putStrLn ""
    putStrLns $ map (\(Tuple o i) → show i <> ") " <> toString o) os
    s ← question "\nEnter number: "
    case A.find (\(Tuple o i) → show i == s) os of
      Nothing          → selectOption options
      Just (Tuple o _) → onOption o
  where
    os = A.zipWith (\o i → Tuple o i) options (A.range 1 100) -- not lazy, so no infinte range - 100 is plenty big!
    toString (ShowInventory _) = "Show inventory"
    toString (Take          _) = "Take an item"
    toString (Use           _) = "Use"
    toString (Examine       _) = "Examine"
    toString (TalkTo      l _) = "Talk to " <> l
    toString (Say         l _) = "Say " <> l
    toString (InventoryO  l _) = l
    toString (ExamineO    l _) = l
    toString (TakeO       l _) = l
    toString (UseO        l _) = l
    toString (UseWith     l1 _ l2 _) = "with " <> l2
    toString (Go          l _) = "Go " <> l

    onOption (ShowInventory oids) = onSelectInventory oids
    onOption (Take          oids) = onSelectTake oids
    onOption (Use           oids) = onSelectUse oids
    onOption (Examine       oids) = onSelectExamine oids
    onOption (TalkTo      _ atn) = sayAction atn
    onOption (Say         _ atn) = sayAction atn
    onOption (InventoryO  _ oid) = onSelectInventoryO oid
    onOption (ExamineO    _ oid) = onSelectExamineO oid
    onOption (TakeO       _ oid) = onSelectTakeO oid
    onOption (UseO        _ oid) = onSelectUseO oid
    onOption (UseWith     _ oid1 _ oid2) = onSelectUseWithO oid1 oid2
    onOption (Go          _ atn) = onSelectGo atn

onSelectGo ∷ Action (Maybe Rid) → SS Unit
onSelectGo roomAction = do
  eRid ← goto roomAction
  either putStrLns (\_ → displayRoom) eRid

onSelectInventory ∷ Array Oid → SS Unit
onSelectInventory items = do
  options ← traverse (withObj \(Tuple oid obj) → pure $ InventoryO obj.title oid) items
  selectOption options

onSelectInventoryO ∷ Oid → SS Unit
onSelectInventoryO oid = do
  obj ← toObject oid
  let suffix =
        case obj.use of
          Left  _ → ""
          Right _ → " with "
  selectOption [ ExamineO ("Examine " <> obj.title          ) oid
               , UseO     ("Use "     <> obj.title <> suffix) oid
               ]

onSelectTake ∷ Array Oid → SS Unit
onSelectTake items = do
  options ← traverse (withObj \(Tuple oid obj) → pure $ TakeO obj.title oid) items
  selectOption options

onSelectTakeO ∷ Oid → SS Unit
onSelectTakeO oid = do
  putStrLn ""
  obj ← toObject oid
  putStrLn $ "You take " <> the obj <> "."
  takeItemS oid

onSelectExamine ∷ Array Oid → SS Unit
onSelectExamine items = do
  options ← traverse (withObj \(Tuple oid obj) → pure $ ExamineO obj.title oid) items
  selectOption options

onSelectExamineO ∷ Oid → SS Unit
onSelectExamineO oid = do
  putStrLn ""
  obj   ← toObject oid
  descr ← runAction $ obj.descr
  putStrLn $ "You examine " <> the obj <> "."
  putStrLn $ " " <> intercalate "\n" descr

onSelectUse ∷ Array Oid → SS Unit
onSelectUse items = do
  options ← traverse (withObj \(Tuple oid obj) → pure $ UseO obj.title oid) items
  selectOption options

onSelectUseO ∷ Oid → SS Unit
onSelectUseO oid = do
  obj ← toObject oid
  story ← get
  room  ← currentRoom
  let accessibleItems = A.filter (_ /= oid) ((story ^.sInventory) <> room.items)
  case obj.use of
    Left  _ → onSelectUseItselfO oid
    Right _ → do
              options ← traverse (withObj \(Tuple oid2 obj2) → pure $ UseWith obj.title oid obj2.title oid2) accessibleItems
              selectOption options

onSelectUseWithO ∷ Oid → Oid → SS Unit
onSelectUseWithO oid1 oid2 = do
  putStrLn ""
  o1 ← toObject oid1
  o2 ← toObject oid2
  putStrLn $ "You use " <> the o1 <> " with " <> the o2 <> ". "
  txt ← useWith oid1 oid2
  putStrLns $ if A.null txt
    then ["Nothing happens."]
    else txt

onSelectUseItselfO ∷ Oid → SS Unit
onSelectUseItselfO oid = do
  putStrLn ""
  o ← toObject oid
  putStrLn $ "You use " <> the o <> ". "
  res ← useItself oid
  case res of
    Left error → pure unit -- TODO log error!
    Right []   → putStrLns ["Nothing happens."]
    Right txt  → putStrLns txt

sayAction ∷ Action Unit → SS Unit
sayAction action = do
  modify_ $ sSay .~ []
  txt   ← runAction action
  putStrLns txt
  story ← get
  let nextSayOptions = story ^. sSay
  if not $ A.null nextSayOptions
    then selectOption $ map (\(Tuple s atn) → Say ("Say \"" <> s <> "\"") atn) nextSayOptions
    else putStrLn "You have nothing to say."
