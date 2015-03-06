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