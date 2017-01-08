### next [YYYY.MM.DD]
* Add `TextShow(1)` instances for `Data.Graph.SCC`
* `TextShow.Instances` no longer reexports the entirety of `TextShow`. Doing so meant that `text-show-instances` would be burdened with bumping its major version number every time that `text-show` made an API change in `TextShow` in order to follow the PVP.
* Remove `TextShow.GHC.PackageDb` module
* Refactor test suite

## 3.4
* Require `text-show-3.4` or later
* Update testsuite to be buildable with `text-show-3.4`

## 3.3
* Add the `TextShow.GHC.LanguageExtensions.Type` and `TextShow.GHC.PackageDb` modules, which define instances if using `ghc-boot`
* Add `TextShow` instances for `NameFlavour`, `NameSpace`, `PatSynArgs`, and `PatSynDir` in `TextShow.Language.Haskell.TH`
* Require `text-show-3.3` or later, which has slightly different TH derivation behavior. As a result, the context for the `TextShow1` instance for `Clown` in `TextShow.Data.Bifunctor` had to be changed slightly.
* Allow building with `QuickCheck-2.9`
* Fix GHC HEAD build

### 3.2.1
* Fix build with GHC 8.0
* Add `TextShow` instance for `Overlap` in `TextShow.Language.Haskell.TH`

## 3.2
* Allow build with `text-show-3.2`

### 3.0.1
* Require quickcheck-instances >= 0.3.12 in test suite due to presence of new orphan Arbitrary instances for `vector` datatypes

# 3
* GHC 8.0 support
* Rename functions that previously ended with the suffix `-With` to instead have the prefix `lift-`, consistent with `text-show-3`
* Removed the `TextShow.Data.Semigroup` and `TextShow.Data.List.NonEmpty` modules, as they have been moved to `text-show-3` (as part of moving `Semigroup` into `base`)
* Removed the functions for `Compose`, `Product`, and `Sum` in `TextShow.Data.Functor.Trans`, as they have been moved to `text-show-3` (as part of moving them to `base`)
* Add `TextShow`/`TextShow1` instances for `Fix` and `Sum` in `TextShow.Data.Bifunctor`
* Add `TextShow`/`TextShow1` instances for the datatypes in `Text.PrettyPrint.Annotated` (introduced in @pretty-1.1.1.3@)
* Add `TextShow` instances for the new datatypes in @template-haskell-2.11.0.0@

## 2.1
* Reexport the `TextShow` classes and module from `TextShow.Instances`. This helps Haddock readers discover what new instances are added with `text-show-instances`.
* Make `Tagged` instances poly-kinded

## 2.0.1
* Allow building with `vector-0.11` and above. Be aware that the `Show` instances for `Vector` types in `vector-0.11.0` are different from other versions of `vector`.

# 2
* Changed the top-level module name from `Text.Show.Text` to `TextShow`, since the former was extremely verbose without much benefit. As a result, this will break all existing code that depends on `text-show-instances`.

# 1
* Allow building with `text-show-1`. Also changed the monomorphic functions to match the naming conventions introduced in `text-show-1`.
* Added instances for the `bifunctors` library
* Removed `utf8-string` instance, since it wasn't as useful as I had imagined
* Revamped test suite

# 0.4
* Allow building with `text-show-0.8`
* Modules which were previously exported only if using a recent-enough version of GHC/`base` (e.g., `Text.Show.Text.System.Win32`) are now always exposed. If the functionality that the module provides is not available on a given version of GHC/`base`, the module will not expose anything.
* Change test-suite to use `hspec`, allowing for it to be built on GHC 7.0 and 7.2

### 0.3.0.1
* Fixed tests on Windows

# 0.3
* Added `showbArgPrec` (and corresponding `Show` and `Show1` instances) to `Text.Show.Text.Data.Semigroup`
* Added `Show1` instances for data types in `Text.Show.Text.Data.List.NonEmpty` and `Text.Show.Text.Data.Semigroup`
* Added `showbDoc` (and corresponding `Show` instance) to `Text.Show.Text.Language.Haskell.TH`
* Renamed `showbDoc` in `Text.Show.Text.Text.PrettyPrint` to `renderB`

# 0.2
* Exposed `showbKindPrec` and `showbPredPrec` with later versions of Template Haskell
* Added `renderStyleB` to `Text.Show.Text.Text.PrettyPrint` (for `time-1.5` and later)
* Added `showbTimeLocalePrec` to `Text.Show.Text.Data.Time`
* Added instances for the `binary`, `haskeline`, `hoopl`, `terminfo`, and `utf8-string` libraries
* Removed `transformers-four` flag

# 0.1
* Initial commit
