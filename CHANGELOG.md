# 1
* Allow building with `text-show-1`. Also changed the monomorphic functions to match the naming conventions introduced in `text-show-1`.
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
