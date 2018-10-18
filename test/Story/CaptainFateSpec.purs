module Story.CaptainFateSpec where

import Prelude

import Control.Monad.State (evalState)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Motor.History as H
import Motor.Interpreter.StoryInterpreter (buildStory)
import Motor.Story (StoryBuilder)
import Story.CaptainFate (story, walkthrough)
import Test.Unit (TestSuite, failure, test)
import Test.Unit.Assert (assert)

spec ∷ TestSuite
spec = test "run walkthrough" do
  let completePath = intercalate "\n" $ A.reverse walkthrough
  case initStory story completePath of
    Left err -> failure err
    Right { historicTxt, initTxt } -> do
      liftEffect $ log (intercalate "\n\n" historicTxt)
      assert "replay of walkthrough doesn't break" $ [] == initTxt

initStory ∷ StoryBuilder Unit → String → Either String { historicTxt ∷ Array String
                                                       , initTxt     ∷ Array String
                                                       }
initStory storyBuilder path = do
  story ← buildStory storyBuilder
  lmap _.error $ evalState (H.initStory path) story
