# stratigraphr (development version)

- `strat_is_valid()` renamed to `strg_is_valid()` (#21)
- Adds `print()` method for stratigraphs
- Minimum supported R version is now 4.1
- Removes deprecated `c14_*` and `cal_*` functions. These now live in the
  [c14 package](https://c14.joeroe.io/).
  - Removes `shub1_radiocarbon` dataset (also now found in c14)
- Adds dependencies on vctrs, vroom and c14 (suggested)
- Drops dependencies on rcarbon, magrittr, tidyr, Bchron (suggested) and 
  ggplot2 (suggested)
- Downgrades dependencies on tibble, dplyr and readr to suggested

# stratigraphr 0.4.0

- `strg_prune()` implements transitive reduction of stratigraphic graphs (#13)
- `strg_locate_cycles()` locates cycles in stratigraphic graphs (#3)
- Radiocarbon-related functions (`c14_*` and `cal_*`) have been moved to the
  [c14 package](https://github.com/joeroe/c14). The legacy versions in this
  package will issue a deprecation warning until the next version, when they
  will be removed.
- Removes fs dependency – replaced with base R functions (#16)
- Removes tidyselect dependency
- ggplot2 and ggraph moved to suggested dependencies

# stratigraphr 0.3.0

- Expanded framework for tidy radiocarbon analysis, adding:
  - New S3 object `cal`, a generic representation of calibrated probability distributions
  - Methods for printing and summarising `cal` objects
  - Type conversion functions for `cal` objects (`as_cal.CalDates()`,
    `as_cal.oxcAARCalibratedDatesList()` and `as_cal.BchronCalibratedDates()`)
  - New `vignette("tidy_radiocarbon")`
- **Breaking change**: `cal()` and `sum()` renamed `c14_calibrate` and
  `c14_sum` and now return `cal` objects.
- **Breaking changes** to `stratigraph` interface:
  - Renamed functions: `harris()` → `strat_connect()`, `is_valid_harris()` →
    `strat_is_valid()`
  - Removed the notion of a "type" of `stratigraph()`.
  - `stratigraph()` will now return a graph even if it is not stratigraphically
    valid.
  - Renamed `vignette("harris")` → `vignette("stratigraph")`
- Expanded the `stratigraph` interface, adding:
  - Utility functions for validating stratigraphies: `strat_is_mirror()`
- Fixed read_lst() when reading Stratify 'extended' LST files
- Added a `NEWS.md` file to track changes to the package.

# stratigraphr 0.2.0

Initial alpha release, including:

- Functions for constructing stratigraphic graphs
- Simple tidy wrappers for the rcarbon package
- Partial CQL interface
