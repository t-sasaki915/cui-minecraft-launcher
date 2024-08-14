module Data.Table
    ( MaybeData (..)
    , StringData (..)
    , Table1 (..)
    , Table2 (..)
    , Table3 (..)
    , Table4 (..)
    , Table5 (..)
    , createTable1
    , createTable2
    , createTable3
    , createTable4
    , createTable5
    , addDataList
    , addData
    ) where

import           Control.Monad.Trans.State.Strict (StateT, execState, get, put)
import           Data.Functor.Identity            (Identity (..))
import           Data.List                        (zipWith4, zipWith5)
import           Data.Table.Internal
import           Text.Printf                      (printf)

data MaybeData a = Some a | NoData

instance Show a => Show (MaybeData a) where
    show (Some a) = show a
    show NoData   = "N/A"

newtype StringData = StringData String

instance Show StringData where
    show (StringData str) = str

data Table1 a         = Table1 String                             [a]
data Table2 a b       = Table2 String String                      [(a, b)]
data Table3 a b c     = Table3 String String String               [(a, b, c)]
data Table4 a b c d   = Table4 String String String String        [(a, b, c, d)]
data Table5 a b c d e = Table5 String String String String String [(a, b, c, d, e)]

createTable1 :: String -> StateT [a] Identity () -> Table1 a
createTable1 desc1 = Table1 desc1 . flip execState []

createTable2 :: String -> String -> StateT [(a, b)] Identity () -> Table2 a b
createTable2 desc1 desc2 = Table2 desc1 desc2 . flip execState []

createTable3 :: String -> String -> String -> StateT [(a, b, c)] Identity () -> Table3 a b c
createTable3 desc1 desc2 desc3 = Table3 desc1 desc2 desc3 . flip execState []

createTable4 :: String -> String -> String -> String -> StateT [(a, b, c, d)] Identity () -> Table4 a b c d
createTable4 desc1 desc2 desc3 desc4 = Table4 desc1 desc2 desc3 desc4 . flip execState []

createTable5 :: String -> String -> String -> String -> String -> StateT [(a, b, c, d, e)] Identity () -> Table5 a b c d e
createTable5 desc1 desc2 desc3 desc4 desc5 = Table5 desc1 desc2 desc3 desc4 desc5 . flip execState []

addDataList :: [a] -> StateT [a] Identity ()
addDataList xs = get >>= put . (++ xs)

addData :: a -> StateT [a] Identity ()
addData x = addDataList [x]

separator :: Int -> String
separator = flip replicate '-'

instance Show a => Show (Table1 a) where
    show (Table1 desc1 elems) =
        let strElems = map show elems
            maxWidth = maximum (map length (desc1 : strElems))
            formatter = printf "| %%-%ds |" maxWidth in
                unlines
                    ( printf formatter desc1
                    : printf "| %s |" (replicate maxWidth '-')
                    : map (printf formatter) strElems
                    )

instance (Show a, Show b) => Show (Table2 a b) where
    show (Table2 desc1 desc2 elems) =
        let strElems1 = map (show . fst) elems
            strElems2 = map (show . snd) elems
            maxWidth1 = maximum (map length (desc1 : strElems1))
            maxWidth2 = maximum (map length (desc2 : strElems2))
            formatter = printf "| %%-%ds | %%-%ds |" maxWidth1 maxWidth2 in
                unlines
                    ( printf formatter desc1 desc2
                    : printf "| %s | %s |" (separator maxWidth1) (separator maxWidth2)
                    : zipWith (printf formatter) strElems1 strElems2
                    )

instance (Show a, Show b, Show c) => Show (Table3 a b c) where
    show (Table3 desc1 desc2 desc3 elems) =
        let strElems1 = map (show . fst3) elems
            strElems2 = map (show . snd3) elems
            strElems3 = map (show . thd3) elems
            maxWidth1 = maximum (map length (desc1 : strElems1))
            maxWidth2 = maximum (map length (desc2 : strElems2))
            maxWidth3 = maximum (map length (desc3 : strElems3))
            formatter = printf "| %%-%ds | %%-%ds | %%-%ds |" maxWidth1 maxWidth2 maxWidth3 in
                unlines
                    ( printf formatter desc1 desc2 desc3
                    : printf "| %s | %s | %s |" (separator maxWidth1) (separator maxWidth2) (separator maxWidth3)
                    : zipWith3 (printf formatter) strElems1 strElems2 strElems3
                    )

instance (Show a, Show b, Show c, Show d) => Show (Table4 a b c d) where
    show (Table4 desc1 desc2 desc3 desc4 elems) =
        let strElems1 = map (show . fst4) elems
            strElems2 = map (show . snd4) elems
            strElems3 = map (show . thd4) elems
            strElems4 = map (show . fourth4) elems
            maxWidth1 = maximum (map length (desc1 : strElems1))
            maxWidth2 = maximum (map length (desc2 : strElems2))
            maxWidth3 = maximum (map length (desc3 : strElems3))
            maxWidth4 = maximum (map length (desc4 : strElems4))
            formatter = printf "| %%-%ds | %%-%ds | %%-%ds | %%-%ds |" maxWidth1 maxWidth2 maxWidth3 maxWidth4 in
                unlines
                    ( printf formatter desc1 desc2 desc3 desc4
                    : printf "| %s | %s | %s | %s |" (separator maxWidth1) (separator maxWidth2) (separator maxWidth3) (separator maxWidth4)
                    : zipWith4 (printf formatter) strElems1 strElems2 strElems3 strElems4
                    )

instance (Show a, Show b, Show c, Show d, Show e) => Show (Table5 a b c d e) where
    show (Table5 desc1 desc2 desc3 desc4 desc5 elems) =
        let strElems1 = map (show . fst5) elems
            strElems2 = map (show . snd5) elems
            strElems3 = map (show . thd5) elems
            strElems4 = map (show . fourth5) elems
            strElems5 = map (show . fifth5) elems
            maxWidth1 = maximum (map length (desc1 : strElems1))
            maxWidth2 = maximum (map length (desc2 : strElems2))
            maxWidth3 = maximum (map length (desc3 : strElems3))
            maxWidth4 = maximum (map length (desc4 : strElems4))
            maxWidth5 = maximum (map length (desc5 : strElems5))
            formatter = printf "| %%-%ds | %%-%ds | %%-%ds | %%-%ds | %%-%ds |" maxWidth1 maxWidth2 maxWidth3 maxWidth4 maxWidth5 in
                unlines
                    ( printf formatter desc1 desc2 desc3 desc4 desc5
                    : printf "| %s | %s | %s | %s | %s |" (separator maxWidth1) (separator maxWidth2) (separator maxWidth3) (separator maxWidth4) (separator maxWidth5)
                    : zipWith5 (printf formatter) strElems1 strElems2 strElems3 strElems4 strElems5
                    )
