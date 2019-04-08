# fused-effects-exceptions

This package provides functionality to handle exceptions thrown in the `IO` monad. It delegates to `catch` from `Control.Exception`. An additional `catchSync` primitive is provided to handle the common case of catching only synchronous exceptions.

This implementation was extracted from one originally written by Josh Vera. It requires a version of `fused-effects` later than 0.3.
