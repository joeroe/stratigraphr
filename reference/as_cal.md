# Convert an object to a cal object

`as_cal()` converts objects from other packages that represent
calibrated radiocarbon dates to `cal` objects. Methods are currently
implemented for:

- `CalDates`: from
  [`rcarbon::calibrate()`](https://rdrr.io/pkg/rcarbon/man/calibrate.html)

- `oxcAARCalibratedDate` and `oxcAARCalibratedDatesList`: from
  [`oxcAAR::oxcalCalibrate()`](https://rdrr.io/pkg/oxcAAR/man/oxcalCalibrate.html)

- `BchronCalibratedDates`: from
  [`Bchron::BchronCalibrate()`](https://andrewcparnell.github.io/Bchron/reference/BchronCalibrate.html)

These functions are intended for complex S3 objects from other packages.
See [`cal()`](cal.md) for a more generic constructor, e.g. using a data
frame.

## Usage

``` r
as_cal(x)

# S3 method for class 'CalDates'
as_cal(x)

# S3 method for class 'oxcAARCalibratedDatesList'
as_cal(x)

# S3 method for class 'oxcAARCalibratedDate'
as_cal(x)

# S3 method for class 'BchronCalibratedDates'
as_cal(x)
```

## Arguments

- x:

  object to be converted to a `cal` object.

## Value

`cal` object: a data frame with two columns, `year` and `p`,
representing the calibrated probability distribution. All other values
are stored as attributes and can be accessed with
[`cal_metadata()`](cal_metadata.md).

## See also

Other functions for working with `cal` objects:
[`as.CalDates.cal()`](as.CalDates.cal.md), [`cal()`](cal.md),
[`cal_metadata()`](cal_metadata.md)
