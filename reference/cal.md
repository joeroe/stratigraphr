# Calibrated radiocarbon dates

`cal()` constructs a generic representation of a calibrated radiocarbon
date or other calendar-based probability distribution.

## Usage

``` r
cal(
  x,
  era = c("cal BP"),
  lab_id = NA,
  cra = NA,
  error = NA,
  curve = NA,
  reservoir_offset = NA,
  reservoir_offset_error = NA,
  calibration_range = NA,
  F14C = NA,
  normalised = NA,
  p_cutoff = NA
)

# S3 method for class 'cal'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` with two columns, with calendar years and associated
  probabilities.

- era:

  (Optional) `character`. Calendar system used. Default: `cal BP`.

- lab_id:

  (Optional) `character`. Lab code or other label for the calibrated
  sample.

- cra:

  (Optional) `integer`. Uncalibrated conventional radiocarbon age (CRA)
  of the sample.

- error:

  (Optional) `integer`. Error associated with the uncalibrated sample.

- curve:

  (Optional) `character`. Atmospheric curve used for calibration, e.g.
  "intcal20".

- reservoir_offset:

  (Optional) `integer`. Marine reservoir offset used in the calibration,
  if any.

- reservoir_offset_error:

  (Optional) `integer`. Error associated with the marine reservoir
  offset.

- calibration_range:

  (Optional) `integer` vector of length 2. The range of years over which
  the calibration was performed, i.e. `c(start, end)`.

- F14C:

  (Optional) `logical`. Whether the calibration was calculated using
  F14C values instead of the CRA.

- normalised:

  (Optional) `logical`. Whether the calibrated probability densities
  were normalised.

- p_cutoff:

  (Optional) `numeric`. Lower threshold beyond which probability
  densities were considered zero.

- ...:

  (Optional) Arguments based to other functions.

## Value

`cal` object: a data frame with two columns, `year` and `p`,
representing the calibrated probability distribution. All other values
are stored as attributes and can be accessed with
[`cal_metadata()`](cal_metadata.md).

## See also

Other tidy radiocarbon functions: [`c14_calibrate()`](c14_calibrate.md),
[`c14_sum()`](c14_sum.md)

Other functions for working with `cal` objects:
[`as.CalDates.cal()`](as.CalDates.cal.md), [`as_cal()`](as_cal.md),
[`cal_metadata()`](cal_metadata.md)
