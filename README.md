# fused-effects-exceptions

[![Hackage](https://img.shields.io/hackage/v/fused-effects-exceptions.svg)](https://hackage.haskell.org/package/fused-effects-exceptions)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build Status](https://action-badges.now.sh/fused-effects/fused-effects-exceptions)](https://github.com/fused-effects/fused-effects-exceptions/actions)

This package provides `Control.Effect.Exception`, a module that wraps the [`Control.Exception`](http://hackage.haskell.org/package/base/docs/Control-Exception.html) API from `base` with the vocabulary provided by the [`fused-effects`](http://hackage.haskell.org/package/fused-effects) library. These functions interact with GHC's support for dynamic exceptions, including functions like `catch` for exception handling and `bracket` for resource management.

Please be aware that injudicious use of these functions may provoke surprising interactions with carriers that thread a monadic state as a parameter, à la the `Control.Carrier.State` types provided by `fused-effects`. For example, a function like `finally`, which does not thread any state from its body to its handler block, may discard state writes in cleanup handlers:

```haskell
λ runM (runResource (runState 'a' (modify (succ @Char) `finally` modify (succ . succ @Char))))
('b', ())
```

If this behavior is a concern, a `Control.Carrier.State.IORef` carrier is provided, which fixes this issue given access to a `MonadIO` constraint. If it is not a concern (such as if the cleanup block is only run for its effects in `IO`), then the `StateC` carriers from `fused-effects` will suffice. For more information about the issues associated with this approach, consult Alexis King's excellent [Demystifying `MonadBaseControl`](https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/).

Prior versions of this package provided a `Catch` effect; this has been excised in favor of the more-general `Control.Effect.Exception`, which provides more functionality without requiring any additional carriers beyond a `Lift IO` effect.
