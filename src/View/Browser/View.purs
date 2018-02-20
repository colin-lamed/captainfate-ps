module View.Browser.View (view) where

import Prelude
import Browser.WebStorage (WEB_STORAGE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State (evalState, runState, modify, get)
import Data.Array (concatMap, null)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Element (setScrollTop)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, ReactSpec, ReactThis, createClass, createFactory, readState, spec)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Motor.History as H
import Motor.Interpreter.StoryInterpreter (buildStory)
import Motor.Interpreter.ActionInterpreter (runAction)
import Motor.Story (Action, DirHint(..), Oid, Rid, StoryBuilder)
import Motor.Util (currentRoom, goto, listExits, takeItemS, the, toObject, toRoom, roomDesc, useItself, useWith)
import Motor.Lens (pLocation, sInventory, sMaxScore, sPlayer, sSay, sScore, sTitle, (.~), (^.))
import View.Browser.History (readPath, writePath, updateHistory)
import View.Browser.Types (AppState, Option(..), SS, addText, clearText, initOptions, resetOptions, runSS, setOptions)
import View.Browser.Utils (getOffsetHeight)


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
            txt → txt
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
  txts ← do modify $ sSay .~ []
            runAction atn
  addText txts
  story ← get
  let sayOptions = story ^. sSay
  if null sayOptions
    then do addText ["You have nothing to say."]
            resetOptions
    else setOptions $ map (\(Tuple l atn) → Say l atn) sayOptions
  pure unit

initState
  ∷ ∀ eff
  . StoryBuilder Unit
  → String
  → Eff ( console    ∷ CONSOLE
        , webStorage ∷ WEB_STORAGE
        | eff
        ) AppState
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
  ∷ ∀ props eff
  . ReactThis props AppState
  → props
  → AppState
  → Eff (dom ∷ DOM | eff) Unit
afterComponentUpdate ctx _ state = do
  doc ← window >>= document

  -- mElmt2 ← getElementById (ElementId "old-text") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  -- let elmt2 = unsafePartial fromJust mElmt2
  oh ← getOffsetHeight "old-text"

  mElmt ← getElementById (ElementId "text-area") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  let elmt = unsafePartial fromJust mElmt
  setScrollTop oh elmt
  pure unit

mainContent ∷ ∀ props eff. AppState → ReactSpec props AppState ReactElement eff
mainContent state0 = do
  spec state0 \ctx → do
    {story, ui} ← readState ctx
    let rid = story ^. (sPlayer <<< pLocation)
        r   = evalState (toRoom rid) story

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
                                                                          , P.onClick \_ → runSS ctx $ exitAction label rid
                                                                          ]
                                                                          [ D.text label]
                                                                     ]


        --renderTextArea ∷ D.ReactElement
        renderTextArea = D.div' $ [  D.span [ P._id "old-text"
                                            , P.style { color: "rgb(80,80,80)" }
                                            ] $ concatMap (\txt → [D.text txt, D.br' [], D.br' []]) ui.txt
                                  ,  D.span [P.style { color: "rgb(0,0,0)"    }] $ concatMap (\txt → [D.text txt, D.br' [], D.br' []]) ui.newTxt
                                  ] <> exitsText

        -- Buttons

        renderButton { buttonLabel, buttonAction } =
          D.button [ P.className "btn btn-lg btn-default"
                   , P._type "button"
                   , P.onClick \_ → buttonAction
                   ]
                   [ D.text buttonLabel ]

        title oid = (evalState (toObject oid) story).title


        toOption (ShowInventory items) = { buttonLabel: "Show inventory"        , buttonAction: runSS ctx $ onClickInventory  items}
        toOption (Take          items) = { buttonLabel: "Take an item"          , buttonAction: runSS ctx $ onClickTake       items}
        toOption (Use           items) = { buttonLabel: "Use"                   , buttonAction: runSS ctx $ onClickUse        items}
        toOption (Examine       items) = { buttonLabel: "Examine"               , buttonAction: runSS ctx $ onClickExamine    items}
        toOption (TalkTo        l atn) = { buttonLabel: "Talk to " <> l         , buttonAction: runSS ctx $ onClickTalkTo     l atn}
        toOption (Say           l atn) = { buttonLabel: "Say \"" <> l <> "\""   , buttonAction: runSS ctx $ onClickSay        l atn}
        toOption (InventoryO      oid) = { buttonLabel: title oid               , buttonAction: runSS ctx $ onClickInventoryO oid}
        toOption (ExamineO        oid) = { buttonLabel: "Examine " <> title oid , buttonAction: runSS ctx $ onClickExamineO   oid}
        toOption (TakeO           oid) = { buttonLabel: "Take "    <> title oid , buttonAction: runSS ctx $ onClickTakeO      oid}
        toOption (UseO            oid) = { buttonLabel: "Use "     <> title oid , buttonAction: runSS ctx $ onClickUseO       oid}
        toOption (UseWith   oid1 oid2) = { buttonLabel: "With "    <> title oid2, buttonAction: runSS ctx $ onClickUseWith    oid1 oid2}

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

view
  ∷ StoryBuilder Unit
  → Eff ( console    ∷ CONSOLE
        , dom        ∷ DOM
        , webStorage ∷ WEB_STORAGE
        ) Unit
view sb = do
  path ← readPath
  state0 ← initState sb path

  let --spec0 ∷ ∀ props render eff. ReactSpec props AppState render eff
      spec0 = mainContent state0
  let --spec1 ∷ ∀ props render eff. ReactSpec props AppState render eff
      spec1 = spec0 { componentDidUpdate = afterComponentUpdate }

  let component = D.div [] [ createFactory (createClass spec1) unit ]
  doc ← window >>= document
  ctr ← getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  _   ← render component (unsafePartial fromJust ctr)
  pure unit
