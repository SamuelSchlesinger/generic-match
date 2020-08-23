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
module Generic.Match (match, Matcher) where

import Data.Foldable
import GHC.Generics
import Data.Void
import Prelude hiding (const)

-- | A first class pattern matching function for anything 'Generic', in the style of 'either' and
-- 'maybe'.
match :: forall b r a x0 x1 x2 x3. (Generic b, Match a r, Rep b ~ D1 ('MetaData x0 x1 x2 x3) a) => b -> Matcher b r
match (from -> M1 a) = match' @a @r a

-- | The type of a first class pattern match, having consumed the input.
type Matcher x r = Matcher' (StripData (Rep x)) r

type family StripData g where
  StripData (D1 ('MetaData x0 x1 x2 x3) d) = d


type family Matcher' x r where
  Matcher' V1 r = r
  Matcher' (C1 ('MetaCons x4 x5 x6) a) r = Consumer a r -> r
  Matcher' (C1 ('MetaCons x4 x5 x6) a :+: b) r = Consumer a r -> Matcher' b r

class Match g r where
  match' :: forall x. g x -> Matcher' g r
  const :: r -> Matcher' g r

instance Match V1 r where
  match' x = case x of
  const = id

instance Consume a => Match (C1 ('MetaCons x4 x5 x6) a) r where
  match' (M1 a) f = consume a f
  const r _ = r

instance (Consume a, Match b r) => Match (C1 ('MetaCons x4 x5 x6) a :+: b) r where
  const r _ = const @b r
  match' (L1 (M1 a)) = \c -> const @b @r (consume @a a c)
  match' (R1 b) = \_ -> match' @b @r b

type family Consumer x r where
  Consumer U1 r = r
  Consumer (S1 ('MetaSel x0 x1 x2 x3) (Rec0 x)) r = x -> r
  Consumer (S1 ('MetaSel x0 x1 x2 x3) (Rec0 x) :*: y) r = x -> Consumer y r

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

tests :: Bool
tests = and
  [ match True False True
  , match False True False
  , match (Left (5 :: Int)) (== 5) undefined
  , match (Right ([1,2] :: [Int])) undefined ((== 2) . length)
  , match (Clap 0 True) (\i b -> i == 0 && b) undefined undefined
  , match (X 1) (\x -> x == 1)
  ]
