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
Description: First class pattern matching for GHC.Generics.
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
, StripData
, Matcher'
, Consumer
-- * Re-exported from GHC.Generics
, Generic
) where

import Data.Foldable
import GHC.Generics
import Data.Void
import Prelude

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
--  deriving Generic
-- 
-- newtype X = X { unX :: Int } deriving Generic
--
-- data Klop = Cloop Klop
--  deriving Generic
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
--   deriving Generic
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
match :: forall b r a x0 x1 x2 x3. (Generic b, Match a r, Rep b ~ D1 ('MetaData x0 x1 x2 x3) a) => b -> Matcher b r
match (from -> M1 a) = match' @a @r a      

-- | The type of a first class pattern match, having consumed the input.
type Matcher x r = Matcher' (StripData (Rep x)) r

-- | The type family that strips the 'MetaData' off of a 'GHC.Generics'
-- 'Rep'resentation.
type family StripData g where
  StripData (D1 ('MetaData x0 x1 x2 x3) d) = d

-- | The utility family which defines a 'Matcher', after stripping the
-- metadata from the top level of the 'GHC.Generics' 'Rep'resentation..
type family Matcher' x r where
  Matcher' V1 r = r
  Matcher' (C1 ('MetaCons x4 x5 x6) a) r = Consumer a r -> r
  Matcher' (C1 ('MetaCons x4 x5 x6) a :+: b) r = Consumer a r -> Matcher' b r

-- | The class that is used to inductively define the pattern matching for
-- a particular generic type.
class Match g r where
  match' :: forall x. g x -> Matcher' g r
  const' :: r -> Matcher' g r

instance Match V1 r where
  match' x = case x of
  const' = id

instance Consume a => Match (C1 ('MetaCons x4 x5 x6) a) r where
  match' (M1 a) f = consume a f
  const' r _ = r

instance (Consume a, Match b r) => Match (C1 ('MetaCons x4 x5 x6) a :+: b) r where
  const' r _ = const' @b r
  match' (L1 (M1 a)) = \c -> const' @b @r (consume @a a c)
  match' (R1 b) = \_ -> match' @b @r b

-- | The type family that describes how to consume a product inside of a 'Generic' type.
type family Consumer x r where
  Consumer U1 r = r
  Consumer (S1 ('MetaSel x0 x1 x2 x3) (Rec0 x)) r = x -> r
  Consumer (S1 ('MetaSel x0 x1 x2 x3) (Rec0 x) :*: y) r = x -> Consumer y r

-- | The typeclass used to consume a product inside of a 'Generic' type.
class Consume g where
  consume :: forall r x. g x -> Consumer g r -> r

instance Consume U1 where
  consume U1 r = r

instance Consume (S1 ('MetaSel x0 x1 x2 x3) (Rec0 x)) where
  consume (M1 (K1 x)) f = f x

instance Consume y => Consume (S1 ('MetaSel x0 x1 x2 x3) (Rec0 x) :*: y) where
  consume (M1 (K1 x) :*: y) f = consume y (f x) 

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
  deriving Generic

thingMatcher :: Matcher Thing r ~ ((Bool -> r) -> r) => ()
thingMatcher = ()

pairMatcher :: Matcher (Int, Bool) r ~ ((Int -> Bool -> r) -> r) => ()
pairMatcher = ()

tripleMatcher :: Matcher (Int, Int, Int) r ~ ((Int -> Int -> Int -> r) -> r) => ()
tripleMatcher = ()

voidMatcher :: Matcher Void r ~ r => ()
voidMatcher = ()

data Ploop = Clap Int Bool | Splop [Integer] Float | Flep [Int] [Float] [Bool] deriving Generic

newtype X = X { unX :: Int } deriving Generic

data Klop = Cloop Klop deriving Generic

tests :: Bool
tests = and
  [ match True False True
  , match False True False
  , match (Left (5 :: Int)) (== 5) undefined
  , match (Right ([1,2] :: [Int])) undefined ((== 2) . length)
  , match (Clap 0 True) (\i b -> i == 0 && b) undefined undefined
  , match (X 1) (\x -> x == 1)
  , match (let x = Cloop x in x) (\_ -> True)
  ]
