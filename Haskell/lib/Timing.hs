module Timing where
import System.Clock
import Formatting
import Formatting.Clock

startTiming :: IO TimeSpec
startTiming = getTime Monotonic

showTiming :: TimeSpec -> IO ()
showTiming start = do
    end <- getTime Monotonic
    fprint (timeSpecs) start end