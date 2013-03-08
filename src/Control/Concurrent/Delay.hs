module Control.Concurrent.Delay
    ( delaySeconds
    ) where

import Control.Concurrent

delaySeconds :: Int -> IO ()
delaySeconds n = threadDelay $ 1000 * 1000 * n
