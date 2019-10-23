# 1.0.0.0

* Port to fused-effects 1.0.
* Add `Control.Effect.Resource` and `Control.Carrier.Resource`, as ported from fused-effects 0.5.
* Add `Control.Carrier.State.IORef` to help people migrating from other state carriers.
* Move `Control.Effect.Catch.CatchC` to `Control.Carrier.Catch` and simplify its internals.
* Rename `catch` to `catchAsync` and `catchSync` to `catch`.

# 0.2.0.0

Bump lower bound of `fused-effects` to 0.5.

# 0.1.1.0

Depend on `unliftio-core` for unlifting.

# 0.1.0.0

Initial release.
