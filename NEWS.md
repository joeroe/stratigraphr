# stratigraphr 0.3.0

* Expanded framework for tidy radiocarbon analysis, adding:
  * New S3 object `cal`, a generic representation of calibrated probability distributions
  * Methods for printing and summarising `cal` objects
  * Type conversion functions for `cal` objects
* **Breaking change**: `cal()` and `sum()` renamed `c14_calibrate` and `c14_sum` and now return `cal` objects.
* Added a `NEWS.md` file to track changes to the package.

# stratigraphr 0.2.0

Initial alpha release, including:

* Functions for constructing stratigraphic graphs
* Simple tidy wrappers for the rcarbon package
* Partial CQL interface