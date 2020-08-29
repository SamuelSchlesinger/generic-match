{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module: Generic.Match
Copyright: (c) Samuel Schlesinger 2020-2024
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: non-portable
Description: First class pattern matching based on generics-sop.
-}
module Generic.Match
(
-- * Pattern match on a 'Generic' type
  match
-- * Type classes
, Match
, Consume
-- * Type families
, Matcher
, Matcher'
, Consumer
-- * Re-exported from Generics.SOP
, Generic
) where

import Data.Foldable
import Data.Kind
import Data.Void
import Prelude
import Generics.SOP
import qualified GHC.Generics as GHC

-- | A first class pattern matching function for anything 'Generic', in the style of 'either' and
-- 'maybe', but with the first argument being the thing you are pattern
-- matching on, as opposed to the last argument.
--
-- @either f g x == match x f g@
--
-- @maybe r f x == match x r f@
--
-- Beyond working for 'Maybe' or 'Either', this function works on just
-- about any type you give to it, as long as that type has a 'Generic'
-- instance. For example, this code is from the tests which are not
-- exported from this file:
--
-- @
-- data Ploop =
--    Clap Int Bool
--  | Splop [Integer] Float
--  | Flep [Int] [Float] [Bool]
--  deriving (GHC.Generic, Generic)
-- 
-- newtype X = X { unX :: Int } deriving (GHC.Generic, Generic)
--
-- data Klop = Cloop Klop
--  deriving (GHC.Generic, Generic)
--
-- tests :: Bool
-- tests = and
--  [ match True False True
--  , match False True False
--  , match (Left (5 :: Int)) (== 5) undefined
--  , match (Right ([1,2] :: [Int])) undefined ((== 2) . length)
--  , match (Clap 0 True) (\i b -> i == 0 && b) undefined undefined
--  , match (X 1) (\x -> x == 1)
--  , match (let x = Cloop x in x) (\_ -> True)
--  ]
-- @
--
-- There are other tests as well, at the type level, which I used to
-- develop this library, and I think it makes sense to display those as
-- well:
--
-- @
-- facts :: ()
-- facts = fold
--   [ unitMatcher
--   , boolMatcher
--   , thingMatcher
--   , pairMatcher
--   , tripleMatcher
--   , voidMatcher
--   ]
--
-- unitMatcher :: Matcher () r ~ (r -> r) => ()
-- unitMatcher = ()
-- 
-- boolMatcher :: Matcher Bool r ~ (r -> r -> r) => ()
-- boolMatcher = ()
-- 
-- data Thing = Thing Bool
--   deriving (GHC.Generic, Generic)
--
-- thingMatcher :: Matcher Thing r ~ ((Bool -> r) -> r) => ()
-- thingMatcher = ()
-- 
-- pairMatcher :: Matcher (Int, Bool) r ~ ((Int -> Bool -> r) -> r) => ()
-- pairMatcher = ()
-- 
-- tripleMatcher :: Matcher (Int, Int, Int) r ~ ((Int -> Int -> Int -> r) -> r) => ()
-- tripleMatcher = ()
-- 
-- voidMatcher :: Matcher Void r ~ r => ()
-- voidMatcher = ()
-- @
--
-- These may look strange to the reader, but the way to read them is that
-- the constraint to the left of the fat arrow must be true if I can
-- instantiate one of the terms in a context without assuming it. As
-- I instantiate all of them in that 'fold' (possibly the only use of the
-- '()' monoid that I can think of, all of these constraints must be true.
-- This allowed me to develop this library by making instances that made
-- each new constraint I added true.
match :: forall b r xs. (Generic b, Match (Code b) r) => b -> Matcher b r
match (from -> SOP xs) = match' @(Code b) @r xs

-- | The type of a first class pattern match, having consumed the input.
type Matcher b r = Matcher' (Code b) r

-- | The utility family which defines a 'Matcher', after stripping the
-- metadata from the top level of the 'GHC.Generics' 'Rep'resentation..
type family Matcher' (xs :: [[Type]]) r where
  Matcher' '[] r = r
  Matcher' (x ': xs) r = Consumer x r -> Matcher' xs r

-- | The class that is used to inductively define the pattern matching for
-- a particular generic type.
class Match xs r where
  match' :: NS (NP I) xs -> Matcher' xs r
  const' :: r -> Matcher' xs r

instance Match '[] r where
  match' x = case x of
  const' = id

instance (Consume x, Match xs r) => Match (x ': xs) r where
  const' r _ = const' @xs r
  match' (Z x) = \c -> const' @xs @r (consume @x x c)
  match' (S xs) = \_ -> match' @xs @r xs

-- | The type family that describes how to consume a product inside of a 'Generic' type.
type family Consumer (xs :: [Type]) (r :: Type) where
  Consumer '[] r = r
  Consumer (x ': xs) r = x -> Consumer xs r

-- | The typeclass used to consume a product inside of a 'Generic' type.
class Consume xs where
  consume :: forall r. NP I xs -> Consumer xs r -> r

instance Consume '[] where
  consume Nil r = r

instance Consume xs => Consume (x ': xs) where
  consume (x :* xs) f = consume xs (f (unI x))

facts :: ()
facts = fold
  [ unitMatcher
  , boolMatcher
  , thingMatcher
  , pairMatcher
  , tripleMatcher
  , voidMatcher
  ]

unitMatcher :: Matcher () r ~ (r -> r) => ()
unitMatcher = ()

boolMatcher :: Matcher Bool r ~ (r -> r -> r) => ()
boolMatcher = ()

data Thing = Thing Bool
  deriving (GHC.Generic, Generic)

thingMatcher :: Matcher Thing r ~ ((Bool -> r) -> r) => ()
thingMatcher = ()

pairMatcher :: Matcher (Int, Bool) r ~ ((Int -> Bool -> r) -> r) => ()
pairMatcher = ()

tripleMatcher :: Matcher (Int, Int, Int) r ~ ((Int -> Int -> Int -> r) -> r) => ()
tripleMatcher = ()

voidMatcher :: Matcher Void r ~ r => ()
voidMatcher = ()

data Ploop = Clap Int Bool | Splop [Integer] Float | Flep [Int] [Float] [Bool] deriving (GHC.Generic, Generic)

newtype X = X { unX :: Int } deriving (GHC.Generic, Generic)

data Klop = Cloop Klop deriving (GHC.Generic, Generic)

data Blango = Koooka Bool Int Float Integer Float Bool Integer Int deriving (GHC.Generic, Generic)

data Klaka = Pooka Int Bool | Lotis Integer | Undo Int | Podango () deriving (GHC.Generic, Generic)

tests :: Bool
tests = and
  [ match True False True
  , match False True False
  , match (Left (5 :: Int)) (== 5) undefined
  , match (Right ([1,2] :: [Int])) undefined ((== 2) . length)
  , match (Clap 0 True) (\i b -> i == 0 && b) undefined undefined
  , match (X 1) (\x -> x == 1)
  , match (let x = Cloop x in x) (\_ -> True)
  , match (Koooka True 0 0 0 0 True 0 0) (\b1 x1 x2 x3 x4 b2 x5 x6 -> b1 && b2 && x1 == 0 && x2 == 0 && x3 == 0 && x4 == 0 && x5 == 0 && x6 == 0)
  , match (Podango ()) undefined undefined undefined (const True)
  ]
