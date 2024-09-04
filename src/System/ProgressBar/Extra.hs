module System.ProgressBar.Extra
    ( simpleProgressBarStyle
    , newSimpleProgressBar
    ) where

import           System.ProgressBar

simpleProgressBarStyle :: Label s -> Style s
simpleProgressBarStyle title =
    Style
        { styleWidth         = TerminalWidth 100
        , stylePrefix        = title
        , styleOpen          = "["
        , styleDone          = '='
        , styleCurrent       = '>'
        , styleTodo          = '.'
        , styleClose         = "]"
        , stylePostfix       = percentage
        , styleEscapeTodo    = const ""
        , styleEscapePrefix  = const ""
        , styleEscapePostfix = const ""
        , styleEscapeOpen    = const ""
        , styleEscapeDone    = const ""
        , styleEscapeCurrent = const ""
        , styleEscapeClose   = const ""
        , styleOnComplete    = WriteNewline
        }

newSimpleProgressBar :: Label () -> Int -> IO (ProgressBar ())
newSimpleProgressBar title num =
    newProgressBar (simpleProgressBarStyle title) 10 (Progress 0 num ())
