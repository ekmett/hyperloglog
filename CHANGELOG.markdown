next [????.??.??]
-----------------
* Remove the `siphash` dependency. Because `siphash` no longer builds on
  GHC 9.2+, we instead bundle the code alongside `hyperloglog`. This allows
  `hyperloglog` to build with 9.2.

0.4.5 [2021.11.16]
------------------
* Drop support for pre-8.0 versions of GHC.
* Allow building with `hashable-1.4.*`.

0.4.4 [2021.02.17]
------------------
* Allow building with `lens-5.*`.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

0.4.3 [2019.09.13]
------------------
* Remove unused `safecopy` dependency.

0.4.2
-----
* Add a library dependency on the `doctests` test suite

0.4.1
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.
* Drop (unused) `hashable-extras` dependency
* Add `NFData` instance for `HyperLogLog`
* Require GHC 7.8 or later

0.4.0.4
-------
* Support `cereal` 0.5 and `safecopy` 0.9.

0.4.0.3
-------
* Fixed doctest issues caused by `vector` 0.11
* Unfortunately the `herbie` changes turned out to be flawed (due to issue mikeizbicki/HerbiePlugin#8). Rolling them back for now.

0.4.0.2
-------
* Fixed a haddock issue caused by the comments in the herbie code.

0.4.0.1
-------
* Added `vector` 0.11 support.
* Incorporated some changes suggested by the HerbiePlugin.

0.3.4
-----
* Support `generic-deriving` 1.8. We no longer incur a `generic-deriving` requirement at all except on GHC < 7.6.

0.3.3.1
-------
* Support `reflection` 2

0.3.3
-----
* Added `bytes` and `binary` instances.
* Compiles warning-free on GHC 7.10.

0.3.2
-----
* Simplified `reifyConfig` internals.

0.3.1
-----
* Added `insertHash`. This enables users of the 0.2
  era version of `hyperloglog` to manually pick the
  hash used and load their old data.
* Builds warning-free on GHC 7.10

0.3.0.1
---
* Constraint bumps for `lens` and `generic-deriving`

0.3
---
* Switched to `SipHash`, so the package actually works.

0.2.3.2
-------
* More `#ifdef` bugfixes

0.2.3.1
-------
* `#ifdef` bugfix

0.2.3
-----
* GHC 7.8 compatibility
* Bumped `cereal` dependency.

0.2.1
-----
* Exported `insert` from `Data.HyperLogLog`.

0.2
---
* Made compatible with `lens` 4

0.1
---
* Ported `Data.Analytics.Approximate.HyperLogLog` from [analytics](http://github.com/analytics) into a separate package.
