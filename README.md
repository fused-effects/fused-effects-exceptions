# fused-effects-exceptions

This package provides functionality to handle exceptions thrown in the `IO` monad. It delegates to `catch` from `Control.Exception`. An additional `catchSync` primitive is provided to handle the common case of catching only synchronous exceptions.

This implementation was extracted from one originally written by Josh Vera. Please be aware that as of the time of this writing it depends on the current git `HEAD` of [fused-effects](https://github.com/robrix/higher-order-effects).
