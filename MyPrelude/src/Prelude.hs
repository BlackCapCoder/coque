module Prelude (

    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry,

    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational, Word,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,

    -- ** Semigroups and Monoids
    Semigroup((<>)),
    Monoid(mempty, mappend, mconcat),

    -- ** Monads and functors
    Functor(fmap, (<$)),

    -- ** Folds and traversals
    Foldable(elem,      -- :: (Foldable t, Eq a) => a -> t a -> Bool
             -- fold,   -- :: Monoid m => t m -> m
             foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
             foldr,     -- :: (a -> b -> b) -> b -> t a -> b
             -- foldr', -- :: (a -> b -> b) -> b -> t a -> b
             foldl,     -- :: (b -> a -> b) -> b -> t a -> b
             -- foldl', -- :: (b -> a -> b) -> b -> t a -> b
             foldr1,    -- :: (a -> a -> a) -> t a -> a
             foldl1,    -- :: (a -> a -> a) -> t a -> a
             maximum,   -- :: (Foldable t, Ord a) => t a -> a
             minimum,   -- :: (Foldable t, Ord a) => t a -> a
             product,   -- :: (Foldable t, Num a) => t a -> a
             sum),      -- :: Num a => t a -> a
             -- toList) -- :: Foldable t => t a -> [a]

    Traversable(traverse, sequenceA, mapM, sequence),

    -- ** Miscellaneous functions
    const, flip, ($), until,
    asTypeOf, error, errorWithoutStackTrace, undefined,
    seq, ($!),

    -- * List operations
    map, (++), filter,
    head, last, tail, init, null, length, (!!),
    reverse,
    -- *** Special folds
    and, or, any, all,
    concat, concatMap,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    -- ** Searching lists
    notElem, lookup,
    -- ** Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError


    , unfoldr

    , module Data.Kind
    , module GHC.TypeNats
    , module Data.Coerce
    , module Data.Maybe
    , module Data.Either
    , module Control.Category
    , module Control.Monad
    , module Control.Applicative
    , module Data.Proxy
    , module Data.Void
    , module Data.Function

    ------------------

    , fi

  ) where

import Data.Function (fix)
import Data.Proxy (Proxy (..))
import Data.Void
import Data.Coerce
import Data.Kind
import GHC.TypeNats (Nat (..), KnownNat (..), natVal, natVal')
import Control.Category
import Control.Monad
import Control.Applicative

import System.IO
import System.IO.Error
import Data.List hiding (map)
import Data.Either
import Data.Foldable    ( Foldable(..), asum )
import Data.Functor     ( (<$>) )
import Data.Maybe
import Data.Traversable ( Traversable(..) )
import Data.Tuple

import GHC.Base hiding ( foldr, mapM, sequence, map, id, (.) )
import Text.Read
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

