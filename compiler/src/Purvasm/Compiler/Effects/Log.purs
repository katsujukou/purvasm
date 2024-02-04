module Purvasm.Compiler.Effects.Log where

import Prelude

import Ansi.Codes (GraphicsParam)
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Effect.Class.Console as Console
import Purvasm.Compiler.Types (LogVerbosity(..))
import Purvasm.Log (LogLevel(..))
import Run (Run, EFFECT)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

class Loggable a where
  toLog :: a -> Doc GraphicsParam

instance Loggable (Doc GraphicsParam) where
  toLog = identity

instance Loggable String where
  toLog = Dodo.text

data Log a = Log LogLevel (Doc GraphicsParam) a

derive instance Functor Log

-- | An effect for recording logs about events in the application
type LOG r = (log :: Log | r)

_log :: Proxy "log"
_log = Proxy

log :: forall a r. Loggable a => LogLevel -> a -> Run (LOG + r) Unit
log level message = Run.lift _log (Log level (toLog message) unit)

debug :: forall a r. Loggable a => a -> Run (LOG + r) Unit
debug = log Debug <<< toLog

info :: forall a r. Loggable a => a -> Run (LOG + r) Unit
info = log Info <<< toLog

warn :: forall a r. Loggable a => a -> Run (LOG + r) Unit
warn = log Warn <<< toLog

error :: forall a r. Loggable a => a -> Run (LOG + r) Unit
error = log Error <<< toLog

interpret :: forall a r. (Log ~> Run r) -> Run (LOG + r) a -> Run r a
interpret handler = Run.interpret (Run.on _log handler Run.send)

-- | Write logs to the terminal only.
handleTerminal :: forall a r. LogVerbosity -> Log a -> Run (EFFECT + r) a
handleTerminal verbosity = case _ of
  Log level message next -> do
    let
      printed = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces $ case level of
        Debug -> Ansi.foreground Ansi.BrightBlack message
        Info -> message
        Warn -> Ansi.foreground Ansi.Yellow (Dodo.text "[WARNING] ") <> message
        Error -> Ansi.foreground Ansi.Red (Dodo.text "[ERROR] ") <> message

    Run.liftEffect case verbosity of
      Quiet -> pure unit
      Normal -> when (level /= Debug) (Console.log printed)
      Verbose -> Console.log printed

    pure next