# semcache 0.1.0

Initial release.

* Exact caching via deterministic request hashing (xxhash64).
* Optional semantic caching via embedding similarity (requires `usearchlite`).
* Persistent on-disk storage with auditable blob files.
* Conservative defaults: semantic reuse is opt-in, strict matching is on.
* `sc_wrap()` for transparent function memoization.
* `sc_prune()` for TTL-based and size-based cache management.
* `sc_stats()` and `sc_read()` for inspection and audit.
