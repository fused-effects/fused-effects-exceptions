# fused-effects-exceptions

[![Hackage](https://img.shields.io/hackage/v/fused-effects-exceptions.svg)](https://hackage.haskell.org/package/fused-effects-exceptions)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build Status](https://action-badges.now.sh/fused-effects/fused-effects-exceptions)](https://github.com/fused-effects/fused-effects-exceptions/actions)

<!--
Setup, hidden from the rendered markdown.

```haskell
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main (module Main) where

import Control.Carrier.State.Strict
import Control.Effect.Exception

main :: IO ()
main = pure ()
```
-->


This package provides `Control.Effect.Exception`, a module that wraps the [`Control.Exception`](http://hackage.haskell.org/package/base/docs/Control-Exception.html) API from `base` with the vocabulary provided by the [`fused-effects`](http://hackage.haskell.org/package/fused-effects) library. These functions interact with GHC's support for dynamic exceptions, including functions like `catch` for exception handling and `bracket` for resource management.

Please be aware that injudicious use of these functions may provoke surprising interactions with carriers that thread a monadic state as a parameter, à la the `Control.Carrier.State` types provided by `fused-effects`. For example, a function like `finally`, which does not thread any state from its body to its handler block, may discard state writes in cleanup handlers:

```haskell
discardsState :: IO Char
discardsState = execState 'a' ((throwIO (userError "urk") `finally` put @Char 'z')
    `catch` (\(_ :: IOException) -> pure ()))
```

Though the `put @Char 'z'` statement is evaluated, its effect is ultimately discarded; the result of executing the above is `'a'`. If this behavior is a concern, use the `Control.Carrier.State.IORef` carrier from `fused-effects` itself.

Prior versions of this package provided a `Catch` effect; this has been excised in favor of the more-general `Control.Effect.Exception`, which provides more functionality without requiring any additional carriers beyond a `Lift IO` effect.
