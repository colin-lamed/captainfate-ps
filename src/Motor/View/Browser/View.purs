module Motor.View.Browser.View
  ( browserView
  ) where

import Prelude
import Control.Monad.State (evalState, runState, modify_, get)
import Data.Array (concatMap, null)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Motor.History as H
import Motor.Interpreter.StoryInterpreter (buildStory)
import Motor.Interpreter.ActionInterpreter (runAction)
import Motor.Story.Lens (sInventory, sMaxScore, sSay, sScore, sTitle, (.~), (^.))
import Motor.Story.Types (Action, DirHint(..), Oid, Rid, StoryBuilder)
import Motor.Util (currentRoom, goto, listExits, takeItemS, the, toObject, roomDesc, useItself, useWith)
import Motor.View.Browser.History (readPath, writePath, updateHistory)
import Motor.View.Browser.Types (AppState, Option(..), SS, addText, clearText, initOptions, resetOptions, runSS, setOptions)
import Motor.View.Browser.Utils (getOffsetHeight)
import Partial.Unsafe (unsafePartial)
import React as R -- (ReactElement, ReactSpec, ReactThis, createClass, createFactory, readState, spec)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Web.DOM.Element (setScrollTop)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


onClickInventory ∷ Array Oid → SS Unit
onClickInventory items =
  setOptions $ map InventoryO items

onClickTake ∷ Array Oid → SS Unit
onClickTake items =
  setOptions $ map TakeO items

onClickUse ∷ Array Oid → SS Unit
onClickUse itemsToUse =
  setOptions $ map UseO itemsToUse

onClickExamine ∷ Array Oid → SS Unit
onClickExamine items = do
  setOptions $ map InventoryO items

onClickInventoryO ∷ Oid → SS Unit
onClickInventoryO oid = do
  setOptions [ExamineO oid, UseO oid]

onClickExamineO ∷ Oid → SS Unit
onClickExamineO oid = do
  obj   ← toObject oid
  story ← get
  descr ← runAction obj.descr
  addText $ ["You examine " <> the obj <> "."] <> descr
  resetOptions
  updateHistory $ H.addExamine (obj.title)
  pure unit

onClickTakeO ∷ Oid → SS Unit
onClickTakeO oid = do
  takeItemS oid
  obj ← toObject oid
  addText ["You take " <> the obj <> "."]
  resetOptions
  updateHistory $ H.addTake (obj.title)
  pure unit

onClickUseO ∷ Oid → SS Unit
onClickUseO oid = do
  obj   ← toObject oid
  story ← get
  room  ← currentRoom
  let accessibleItems = (story ^. sInventory) <> room.items
  case obj.use of
    Left l  → do addText ["You use " <> the obj <> "."]
                 res ← useItself oid
                 case res of
                   Left error → pure unit -- TODO log error!
                   Right []   → addText ["Nothing happens."]
                   Right txt  → addText txt
                 resetOptions
                 updateHistory $ H.addUse (obj.title) Nothing
    Right r → setOptions $ map (\oid2 → UseWith oid oid2) accessibleItems
  pure unit

onClickUseWith ∷ Oid → Oid → SS Unit
onClickUseWith oid1 oid2 = do
  obj1 ← toObject oid1
  obj2 ← toObject oid2
  addText ["You use " <> the obj1 <> " with " <> the obj2 <> "."]
  txt ← useWith oid1 oid2
  addText case txt of
            []  → ["Nothing happens."]
            txt' → txt'
  resetOptions
  updateHistory $ H.addUse (obj1.title) (Just $ obj2.title)
  pure unit

exitAction ∷ String → Action (Maybe Rid) → SS Unit
exitAction label roomAction = do
  res ← goto roomAction
  case res of
    Left txts → addText txts
    Right _   → do clearText
                   txts ← roomDesc
                   addText txts
  resetOptions
  updateHistory $ H.addGo label
  pure unit

onClickTalkTo ∷ String → Action Unit → SS Unit
onClickTalkTo label atn = do
  updateHistory $ H.addTalk label
  sayAction atn

onClickSay ∷ String → Action Unit → SS Unit
onClickSay label atn = do
  updateHistory $ H.addSay label
  sayAction atn

sayAction ∷ Action Unit → SS Unit
sayAction atn = do
  txts ← do modify_ $ sSay .~ []
            runAction atn
  addText txts
  story ← get
  let sayOptions = story ^. sSay
  if null sayOptions
    then do addText ["You have nothing to say."]
            resetOptions
    else setOptions $ map (\(Tuple l atn') → Say l atn') sayOptions
  pure unit

initState
  ∷ StoryBuilder Unit
  → String
  → Effect AppState
initState sb path = do
  let story = buildStory sb
      Tuple res story' = runState (H.initStory path) story
  txts ← case res of
              Right (Tuple _ initTxt) → pure $ initTxt
              Left  (Tuple h err)     → do log $ "Couldn't replay state: " <> err
                                           -- replace history with amount successfully restored
                                           writePath $ H.serialise h
                                           pure []

  -- TODO tidy the following up - note we need to display room on each room display (see exitAction), should we do it outside of initState?
  let Tuple roomTxt story'' = runState roomDesc story'

  let options = evalState initOptions story''

  pure { story: story''
       , ui   : { options: options
                , txt    : []
                , newTxt : txts <> roomTxt
                }
       }

afterComponentUpdate
  ∷ forall snapshot
  . {}
  → AppState
  → snapshot
  → Effect Unit
afterComponentUpdate _ state _ = do
  doc ← window >>= document

  -- mElmt2 ← getElementById "old-text" (toNonElementParentNode doc)
  -- let elmt2 = unsafePartial fromJust mElmt2
  oh ← getOffsetHeight "old-text"

  mElmt ← getElementById "text-area" (toNonElementParentNode doc)
  let elmt = unsafePartial fromJust mElmt
  setScrollTop oh elmt
  pure unit


mainContent ∷ AppState -> R.ReactClass {}
mainContent state0 =
    R.component "Page" component
  where
    component this =
      pure { state              : state0
           , componentDidUpdate : afterComponentUpdate
           , render             : render this
           }
    render this = do
      {story, ui} ← R.getState this
      let r   = evalState currentRoom story

      let --renderRoom ∷ D.ReactElement
          renderRoom     = D.text r.title

          --renderProgress ∷ D.ReactElement
          renderProgress = D.text $ case story ^.sMaxScore of
                                      Just maxScore → show (100 * (story ^.sScore) / maxScore) <> "% completed"
                                      Nothing       → "Score " <> show (story ^.sScore)


          -- list room exits
          exits     = evalState (listExits r) story
          exitsText = case exits of
                        [] → [D.i' [D.text "There are no exits visible"]]
                        _  → [D.i' (   [D.text "The following exits are visible: "]
                                   <> intercalate ([D.text ", "]) (map toHtml exits)
                                   )
                             ]
                                where colour N = "rgb(130,0,186)"
                                      colour W = "rgb(0,100,0)"
                                      colour E = "rgb(0,0,255)"
                                      colour S = "rgb(255,0,0)"
                                      colour U = "rgb(255,0,255)"
                                      toHtml ({label, dirHint, rid}) = [D.a [ P.style { color: (colour dirHint) }
                                                                            , P.onClick \_ → runSS this $ exitAction label rid
                                                                            ]
                                                                            [ D.text label]
                                                                       ]


          --renderTextArea ∷ D.ReactElement
          renderTextArea = D.div' $ [  D.span [ P._id "old-text"
                                              , P.style { color: "rgb(80,80,80)" }
                                              ] $ concatMap (\txt → [D.text txt, D.br', D.br']) ui.txt
                                    ,  D.span [P.style { color: "rgb(0,0,0)"    }] $ concatMap (\txt → [D.text txt, D.br', D.br']) ui.newTxt
                                    ] <> exitsText

          -- Buttons

          renderButton { buttonLabel, buttonAction } =
            D.button [ P.className "btn btn-lg btn-default"
                     , P._type "button"
                     , P.onClick \_ → buttonAction
                     ]
                     [ D.text buttonLabel ]

          title oid = (evalState (toObject oid) story).title


          toOption (ShowInventory items) = { buttonLabel: "Show inventory"        , buttonAction: runSS this $ onClickInventory  items}
          toOption (Take          items) = { buttonLabel: "Take an item"          , buttonAction: runSS this $ onClickTake       items}
          toOption (Use           items) = { buttonLabel: "Use"                   , buttonAction: runSS this $ onClickUse        items}
          toOption (Examine       items) = { buttonLabel: "Examine"               , buttonAction: runSS this $ onClickExamine    items}
          toOption (TalkTo        l atn) = { buttonLabel: "Talk to " <> l         , buttonAction: runSS this $ onClickTalkTo     l atn}
          toOption (Say           l atn) = { buttonLabel: "Say \"" <> l <> "\""   , buttonAction: runSS this $ onClickSay        l atn}
          toOption (InventoryO      oid) = { buttonLabel: title oid               , buttonAction: runSS this $ onClickInventoryO oid}
          toOption (ExamineO        oid) = { buttonLabel: "Examine " <> title oid , buttonAction: runSS this $ onClickExamineO   oid}
          toOption (TakeO           oid) = { buttonLabel: "Take "    <> title oid , buttonAction: runSS this $ onClickTakeO      oid}
          toOption (UseO            oid) = { buttonLabel: "Use "     <> title oid , buttonAction: runSS this $ onClickUseO       oid}
          toOption (UseWith   oid1 oid2) = { buttonLabel: "With "    <> title oid2, buttonAction: runSS this $ onClickUseWith    oid1 oid2}

          --renderButtonArea ∷ D.ReactElement
          renderButtonArea = D.span' $ map (renderButton <<< toOption) ui.options

      pure $
        D.div [ P.className "container"
              , P.role      "main"
              ]
              [ D.div [ P.className "page-header" ]
                      [ D.h1' [ D.text (story ^. sTitle) ] ]
              , D.div [ P.className "row" ]
                      [ D.div [ P.className "panel panel-default" ]
                              [ D.div [ P.className "panel-heading" ]
                                      [ D.table [ P.style { width: "100%" } ]
                                                [ D.tbody' [ D.tr' [ D.td [ P.className "panel-title" ]
                                                                         [ renderRoom ]
                                                                   , D.td [ P.className "text-right" ]
                                                                         [ renderProgress ]
                                                                   ]
                                                ]]
                                      ]
                              , D.div [ P.className "panel-body" ]
                                      [ D.div [ P._id "text-area"
                                              , P.style { width: "100%", height: "50vh", overflow: "auto", fontsize: "130%"} ]
                                              [ renderTextArea ]
                                      ]
                              ]
                      ]
              , D.div [ P.className "row" ]
                      [ D.div' [ renderButtonArea ] ]
              ]

browserView
  ∷ StoryBuilder Unit
  → Effect Unit
browserView sb = do
  path ← readPath
  state0 ← initState sb path
  let component = R.createLeafElement (mainContent state0) {}
  doc ← window >>= document
  ctr ← getElementById "main" (toNonElementParentNode doc)
  _   ← render component (unsafePartial fromJust ctr)
  pure unit
