# generic-match

When I'm writing Haskell code, often I write things like:

```haskell
x <- doThing >>= either errorHandler pure
y <- doOtherThing >>= maybe (throwIO Shriek) pure
```

This comes up in more places than error handling, but I think this is a
sufficient example. There is a compromise one makes with their API, where
they either offer a specific error type, and force you to deconstruct it and
fiddle with it on your own, but usually the names are more descriptive. On
the other hand, with Either or Maybe, we can use all of the standard functions
available for operating on them, such as either and maybe. This package is
getting rid of the cost of entry for deconstructing your own types in this
same style. Now we can write:

```haskell
data DatabaseAccess a =
    ConnectionFailure String
  | InvalidRowCount Int
  | Successful a
  deriving Generic
...
x <- doThing >>= match error (error . show) pure
```

This is the motivating case, but there are many others! For instance, you can
also replace your use of either and maybe with the more "Generic" (heh) match.

```haskell
x <- doThing >>= match errorHandler pure
y <- doOtherThing >>= match (throwIO Shriek) pure
```
