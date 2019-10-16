# fused-effects-exceptions

[![Hackage](https://img.shields.io/hackage/v/fused-effects-exceptions.svg)](https://hackage.haskell.org/package/fused-effects-exceptions)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/patrickt/fused-effects-exceptions.svg)](https://travis-ci.org/patrickt/fused-effects-exceptions)

This package provides two useful effects for handling exceptions thrown from pure code or IO with GHC's `Control.Exception.throw` function.

## Control.Effect.Resource

This effect provides `bracket`, `finally`, and `onException` functions capable of allocating and freeing scarce resources in the presence of GHC's exceptions. It is similar in functionality to the [`resourcet`](http://hackage.haskell.org/package/resourcet) package.

This effect was included in prior versions of `fused-effects`, but has been moved to this package due to the surprising interactions it can have with the `Control.Carrier.State` carriers provided by `fused-effects`. If you use the `ResourceC` and `StateC` effects in conjunction, writes inside a `finally` block may be discarded, since `finally` discards the result of its cleanup handler:

```haskell
λ runM (runResource (runState 'a' (modify (succ @Char) `finally` modify (succ . succ @Char))))
('b', ())
```

If this behavior is a concern, a `Control.Carrier.State.IORef` carrier is provided, which fixes this issue given access to a `MonadIO` constraint. If it is not a concern (such as if the cleanup block is only run for its effects in `IO`), then the `StateC` carriers from `fused-effects` will suffice. For more information about the issues associated with this approach, consult Alexis King's excellent [Demystifying `MonadBaseControl`](https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/).

## Control.Effect.State

This effect is similar to the `MonadCatch` and `MonadThrow` classes provided by the `exceptions` package. It delegates to `catch` from `Control.Exception`. An additional `catchSync` primitive is provided to handle the common case of catching only synchronous exceptions. Its implementation was extracted from one originally written by Josh Vera.

This effect displays the same behavior associated with `Resource` in that carriers like `Control.Carrier.State.Strict` which rely on return types to propagate state may drop state information.
