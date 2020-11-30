# stratigraphr 0.3.0

* Expanded framework for tidy radiocarbon analysis, adding:
  * New S3 object `cal`, a generic representation of calibrated probability distributions
  * Methods for printing and summarising `cal` objects
  * Type conversion functions for `cal` objects (`as_cal.CalDates()`, `as_cal.oxcAARCalibratedDatesList()` and `as_cal.BchronCalibratedDates()`)
  * New `vignette("tidy_radiocarbon")`
* **Breaking change**: `cal()` and `sum()` renamed `c14_calibrate` and `c14_sum` and now return `cal` objects.
* **Breaking changes** to `stratigraph` interface:
  * Renamed functions: `harris()` → `strat_connect()`, `is_valid_harris()` → `strat_is_valid()`
  * Removed the notion of a "type" of `stratigraph()`.
  * `stratigraph()` will now return a graph even if it is not stratigraphically valid.
  * Renamed `vignette("harris")` → `vignette("stratigraph")`
* Expanded the `stratigraph` interface, adding:
  * Utility functions for validating stratigraphies: `strat_is_mirror()`
* Fixed read_lst() when reading Stratify 'extended' LST files
* Added a `NEWS.md` file to track changes to the package.

# stratigraphr 0.2.0

Initial alpha release, including:

* Functions for constructing stratigraphic graphs
* Simple tidy wrappers for the rcarbon package
* Partial CQL interface