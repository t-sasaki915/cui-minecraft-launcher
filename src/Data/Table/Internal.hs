module Data.Table.Internal
    ( fst3
    , snd3
    , thd3
    , fst4
    , snd4
    , thd4
    , fourth4
    , fst5
    , snd5
    , thd5
    , fourth5
    , fifth5
    ) where

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

fourth4 :: (a, b, c, d) -> d
fourth4 (_, _, _, d) = d

fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _, _) = a

snd5 :: (a, b, c, d, e) -> b
snd5 (_, b, _, _, _) = b

thd5 :: (a, b, c, d, e) -> c
thd5 (_, _, c, _, _) = c

fourth5 :: (a, b, c, d, e) -> d
fourth5 (_, _, _, d, _) = d

fifth5 :: (a, b, c, d, e) -> e
fifth5 (_, _, _, _, e) = e
