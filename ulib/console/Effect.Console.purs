-- | ulib SHADOW of `console` (registry 6.1.0), ADR-0068. Interface-identical to upstream
-- | `Effect.Console` (same open module, same names/types) but with **no `foreign import`**: the
-- | console capability is realised over the generic `Purvasm.Stdio` leaves, so `console`'s consumers
-- | compile natively. `log`/`info`/`debug` route to stdout (`writeLine`); `warn`/`error` to stderr
-- | (`writeErrLine`), matching Node's stream choice. Timers, grouping, and `clear` are **no-ops**:
-- | there is no native console object (a documented behaviour change — presentation-only and
-- | value-unobservable). The `Show` helpers (`logShow` …) and `grouped` are unchanged. No core
-- | resolver knows a `Console` name (ADR-0067/0068).
module Effect.Console where

import Control.Bind (discard, bind, pure)
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Purvasm.Stdio (writeErrLine, writeLine)

-- | Write a message to the console (stdout).
log :: String -> Effect Unit
log = writeLine

-- | Write a value to the console, using its `Show` instance to produce a `String`.
logShow :: forall a. Show a => a -> Effect Unit
logShow a = log (show a)

-- | Write a warning to the console (stderr).
warn :: String -> Effect Unit
warn = writeErrLine

-- | Write a warning value to the console, using its `Show` instance.
warnShow :: forall a. Show a => a -> Effect Unit
warnShow a = warn (show a)

-- | Write an error to the console (stderr).
error :: String -> Effect Unit
error = writeErrLine

-- | Write an error value to the console, using its `Show` instance.
errorShow :: forall a. Show a => a -> Effect Unit
errorShow a = error (show a)

-- | Write an info message to the console (stdout).
info :: String -> Effect Unit
info = writeLine

-- | Write an info value to the console, using its `Show` instance.
infoShow :: forall a. Show a => a -> Effect Unit
infoShow a = info (show a)

-- | Write a debug message to the console (stdout).
debug :: String -> Effect Unit
debug = writeLine

-- | Write a debug value to the console, using its `Show` instance.
debugShow :: forall a. Show a => a -> Effect Unit
debugShow a = debug (show a)

-- | Start a named timer. No native console — a no-op (ADR-0068).
time :: String -> Effect Unit
time _ = pure unit

-- | Print the time since a named timer started. No-op (ADR-0068).
timeLog :: String -> Effect Unit
timeLog _ = pure unit

-- | Stop a named timer and print its elapsed time. No-op (ADR-0068).
timeEnd :: String -> Effect Unit
timeEnd _ = pure unit

-- | Clears the console. No-op (ADR-0068).
clear :: Effect Unit
clear = pure unit

-- | Creates a new inline group in the console. No native console — a no-op (ADR-0068).
group :: String -> Effect Unit
group _ = pure unit

-- | Same as `group`, but collapsed by default. No-op (ADR-0068).
groupCollapsed :: String -> Effect Unit
groupCollapsed _ = pure unit

-- | Exits the current inline group. No-op (ADR-0068).
groupEnd :: Effect Unit
groupEnd = pure unit

-- | Perform an effect within the context of an inline group. Calls `group`/`groupEnd` around it
-- | (both no-ops here), so this is just `inner`.
grouped :: forall a. String -> Effect a -> Effect a
grouped name inner = do
  group name
  result <- inner
  groupEnd
  pure result
