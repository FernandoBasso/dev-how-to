module E3e0e0aHelloWorld where

import Prelude
import Effect (Effect)
import Effect.Console (log)

hello :: String -> Effect Unit
hello s = log (s <> "!!!")
