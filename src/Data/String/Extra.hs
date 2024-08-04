module Data.String.Extra (removeLastNewLine) where

removeLastNewLine :: String -> String
removeLastNewLine str =
    case reverse str of
        ('\n' : after) -> reverse after
        _              -> reverse str
