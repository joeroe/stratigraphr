# Changelog

## stratigraphr (development version)

- Minimum supported R version is now 4.1
- Removes deprecated `c14_*` and `cal_*` functions. These now live in
  the [c14 package](https://c14.joeroe.io/).
- Adds dependencies on vctrs and vroom
- Drops dependencies on cli, rcarbon, magrittr, tidyr, Bchron
  (suggested) and ggplot2 (suggested)
- Downgrades dependencies on tibble, dplyr and readr to suggested

## stratigraphr 0.4.0

- [`strg_prune()`](../reference/strg_prune.md) implements transitive
  reduction of stratigraphic graphs
  ([\#13](https://github.com/joeroe/stratigraphr/issues/13))
- `strg_locate_cycles()` locates cycles in stratigraphic graphs
  ([\#3](https://github.com/joeroe/stratigraphr/issues/3))
- Radiocarbon-related functions (`c14_*` and `cal_*`) have been moved to
  the [c14 package](https://github.com/joeroe/c14). The legacy versions
  in this package will issue a deprecation warning until the next
  version, when they will be removed.
- Removes fs dependency – replaced with base R functions
  ([\#16](https://github.com/joeroe/stratigraphr/issues/16))
- Removes tidyselect dependency
- ggplot2 and ggraph moved to suggested dependencies

## stratigraphr 0.3.0

- Expanded framework for tidy radiocarbon analysis, adding:
  - New S3 object `cal`, a generic representation of calibrated
    probability distributions
  - Methods for printing and summarising `cal` objects
  - Type conversion functions for `cal` objects (`as_cal.CalDates()`,
    `as_cal.oxcAARCalibratedDatesList()` and
    `as_cal.BchronCalibratedDates()`)
  - New
    [`vignette("tidy_radiocarbon")`](../articles/tidy_radiocarbon.md)
- **Breaking change**: `cal()` and
  [`sum()`](https://rdrr.io/r/base/sum.html) renamed `c14_calibrate` and
  `c14_sum` and now return `cal` objects.
- **Breaking changes** to `stratigraph` interface:
  - Renamed functions: `harris()` →
    [`strat_connect()`](../reference/strat_connect.md),
    `is_valid_harris()` →
    [`strat_is_valid()`](../reference/strat_is_valid.md)
  - Removed the notion of a “type” of
    [`stratigraph()`](../reference/stratigraph.md).
  - [`stratigraph()`](../reference/stratigraph.md) will now return a
    graph even if it is not stratigraphically valid.
  - Renamed `vignette("harris")` →
    [`vignette("stratigraph")`](../articles/stratigraph.md)
- Expanded the `stratigraph` interface, adding:
  - Utility functions for validating stratigraphies:
    [`strat_is_mirror()`](../reference/strat_is_mirror.md)
- Fixed read_lst() when reading Stratify ‘extended’ LST files
- Added a `NEWS.md` file to track changes to the package.

## stratigraphr 0.2.0

Initial alpha release, including:

- Functions for constructing stratigraphic graphs
- Simple tidy wrappers for the rcarbon package
- Partial CQL interface
