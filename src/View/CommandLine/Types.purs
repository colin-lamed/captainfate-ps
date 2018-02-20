module View.CommandLine.Types
  ( SS
  , runSS
  ) where

import Prelude
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Either (either)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Motor.Story (Story)
import LineReader (READLINE)
import Node.ReadLine (Interface)


newtype SS a = SS (ReaderT Interface
                    (StateT Story
                      (Aff ( console   ∷ CONSOLE
                           , readline  ∷ READLINE
                           , exception ∷ EXCEPTION
                           )))
                  a)

derive instance newtypeSS ∷ Newtype (SS a) _

runSS
  ∷ ∀ a
  . Story
  → Interface
  → SS a
  → Eff ( console   ∷ CONSOLE
        , exception ∷ EXCEPTION
        , readline  ∷ READLINE
        ) Unit
runSS story interface ss =
  runAff_ (either (EC.error <<< show) (\(Tuple unit' story') → EC.log "the end")) $
    runStateT (runReaderT (unwrap ss) interface) story

derive instance functorSS ∷ Functor SS

instance applySS ∷ Apply SS where
  apply (SS f) (SS a) = SS $ apply f a

instance applicativeSS ∷ Applicative SS where
  pure = SS <<< pure

instance bindSS ∷ Bind SS where
  bind (SS m) f = let g = unwrap <<< f
                  in SS $ bind m g

instance monadSS ∷ Monad SS

instance monadStateSS ∷ MonadState Story SS where
  state = SS <<< lift <<< state

instance monadEffSS ∷ MonadEff ( console   ∷ CONSOLE
                               , exception ∷ EXCEPTION
                               , readline  ∷ READLINE
                               ) SS where
  liftEff = SS <<< liftEff

instance monadAffSS ∷ MonadAff ( console   ∷ CONSOLE
                               , exception ∷ EXCEPTION
                               , readline  ∷ READLINE
                               ) SS where
  liftAff = SS <<< liftAff

instance monadAskSS ∷ MonadAsk Interface SS where
  ask = SS ask
