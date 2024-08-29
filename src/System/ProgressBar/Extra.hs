module System.ProgressBar.Extra
    ( simpleProgressBarStyle
    , newSimpleProgressBar
    ) where

import           System.ProgressBar

simpleProgressBarStyle :: Label s -> Style s
simpleProgressBarStyle title =
    Style
        { styleWidth         = TerminalWidth 100
        , styleTodo          = '.'
        , stylePrefix        = title
        , stylePostfix       = percentage
        , styleOpen          = "["
        , styleOnComplete    = WriteNewline
        , styleEscapeTodo    = const ""
        , styleEscapePrefix  = const ""
        , styleEscapePostfix = const ""
        , styleEscapeOpen    = const ""
        , styleEscapeDone    = const ""
        , styleEscapeCurrent = const ""
        , styleEscapeClose   = const ""
        , styleDone          = '='
        , styleCurrent       = '>'
        , styleClose         = "]"
        }

newSimpleProgressBar :: Label () -> Int -> IO (ProgressBar ())
newSimpleProgressBar title num =
    newProgressBar (simpleProgressBarStyle title) 10 (Progress 0 num ())
