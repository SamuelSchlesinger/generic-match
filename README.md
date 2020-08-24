# generic-match

[![Hackage](https://img.shields.io/hackage/v/generic-match.svg)](https://hackage.haskell.org/package/generic-match)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/generic-match.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/generic-match)

## What?
An implementation of first-class pattern matches in Haskell.

## Why?
Oftentimes, when writing Haskell code, we want to branch on multiple cases of
a sum type, such as

```haskell
data TravelMethod
  = Airplane Airport UTCTime
  | Train TrainStation UTCTime
  | Driving
```

For instance, lets say that we want to grab out the time. In Haskell, we can
do this by writing:

```haskell
timeOfTravel :: TravelMethod -> Maybe UTCTime
timeOfTravel = \case
  Airplane _airport time -> Just time
  Train _trainStation time -> Just time
  Driving -> Nothing
```

This is concise, and preferable to many other languages, but in this case we
can do even better using this library.

```haskell
timeOfTravel travelMethod = match travelMethod (Just . flip const) (Just . flip const) Nothing
```

In this example, perhaps we don't save much, but I hope the principle is clear.
The case for using this library is when you want to branch on the contents of
each different sum, and you _already_ have functions or concise combinators to
build functions that handle your inputs. For a Haskeller, this is already
rather familiar, I claim!

```haskell
either l r x == match x l r
maybe n j x == match x n j
```

## Examples

```haskell
data DatabaseAccess a =
    ConnectionFailure String
  | InvalidRowCount Int
  | Successful a
  deriving Generic

doThing :: m (DatabaseAccess Bool)

...

x <- doThing >>= \g -> match g error (error . show) pure
```

## Contribution

Contributions are very welcome! Feel free to create an issue or a PR or
ping me on any platform to chat about whatever, especially improvements to my
libraries.
