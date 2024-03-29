# 1.2.0.0

- Removed `Control.Carrier.State.IORef` in favor of the one now present in `fused-effects`. The minimum requirement for `fused-effects` has been raised so as to minimize breakage. The major version bump is to handle potential (but almost certainly nonexistent) breakage that could occur given injudicious use of `PackageImports`.

# 1.1.0.1

* GHC 9 support.

# 1.1.0.0

* Port to fused-effects 1.1.

# 1.0.0.0

* Port to fused-effects 1.0.
* Add `Control.Effect.Exception`, which wraps the entirety of `base`'s `Control.Exception`.
* Add `Control.Carrier.State.IORef`, a state carrier that does not drop writes.
* Remove `Catch` effect in favor of `Control.Effect.Exception`.

# 0.2.0.0

Bump lower bound of `fused-effects` to 0.5.

# 0.1.1.0

Depend on `unliftio-core` for unlifting.

# 0.1.0.0

Initial release.
