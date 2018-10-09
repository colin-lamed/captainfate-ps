module Story.CaptainFateSpec where

import Prelude
import Control.Monad.State (evalState)
import Data.Foldable (intercalate)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Motor.History as H
import Motor.Story (StoryBuilder)
import Motor.Interpreter.StoryInterpreter (buildStory)
import Partial.Unsafe (unsafeCrashWith)
import Story.CaptainFate (story, walkthrough)
import Test.Assert (assert')


spec ∷ Effect Unit
spec = do
  let Tuple historicTxt txt = either unsafeCrashWith identity $ initStory story (intercalate "\n" $ A.reverse walkthrough)
  log (intercalate "\n\n" historicTxt)
  assert' "replay of walkthrough doesn't break" $ [] == txt --



-- import Control.Monad.Trans.State       (evalState)
--
-- import TextAd.Model.Core
-- import TextAd.Interpreter.StoryBuilder (toStory)

initStory ∷ StoryBuilder Unit → String → Either String (Tuple (Array String) (Array String))
initStory story path =
  either (Left <<< snd) Right $ evalState (H.initStory path) (buildStory story)
