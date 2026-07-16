# Sum radiocarbon dates with tidy syntax

A wrapper for
[`rcarbon::spd()`](https://rdrr.io/pkg/rcarbon/man/spd.html) that takes
a list of calibrated dates rather than a `CalDates` object. This allows
you to use the output of [`c14_calibrate()`](c14_calibrate.md) and take
advantage of tidy summary syntax
([`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
etc.)

## Usage

``` r
c14_sum(cal, time_range = NA, ...)
```

## Arguments

- cal:

  A list of `cal` objects.

- time_range:

  Vector of length 2 indicating the range of calendar dates over which
  to sum. If left `NA`, the maximum range of the `cal` will be used. See
  details.

- ...:

  Optional arguments to be passed to
  [`rcarbon::spd()`](https://rdrr.io/pkg/rcarbon/man/spd.html)

## Value

A `calGrid` object containing the summed probability distribution.

## Details

Unlike [`rcarbon::spd()`](https://rdrr.io/pkg/rcarbon/man/spd.html),
this function will attempt to guess an appropriate time range if it
isn't explicitly specified with `time_range`. It's probably a good idea
to specify it.

## See also

Other tidy radiocarbon functions: [`c14_calibrate()`](c14_calibrate.md),
[`cal()`](cal.md)

## Examples

``` r
data("shub1_radiocarbon")
shub1_radiocarbon %>%
  dplyr::mutate(cal = c14_calibrate(cra, error, normalise = FALSE, verbose = FALSE)) %>%
  dplyr::group_by(phase) %>%
  dplyr::summarise(SPD = c14_sum(cal, spdnormalised = TRUE, verbose = FALSE),
                   .groups = "drop_last")
#> Warning: There were 2 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `cal = c14_calibrate(cra, error, normalise = FALSE, verbose =
#>   FALSE)`.
#> Caused by warning:
#> ! stratigraphr::c14_calibrate() is deprecated. Please use c14::c14_calibrate() instead.
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
#> Warning: There were 34 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `SPD = c14_sum(cal, spdnormalised = TRUE, verbose = FALSE)`.
#> ℹ In group 1: `phase = "Phase 1"`.
#> Caused by warning:
#> ! stratigraphr::c14_sum() is deprecated. Please use c14::c14_sum() instead.
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 33 remaining warnings.
#> # A tibble: 7 × 2
#>   phase   SPD                   
#>   <chr>   <list>                
#> 1 Phase 1 <CalGrid [11,659 × 2]>
#> 2 Phase 2 <CalGrid [560 × 2]>   
#> 3 Phase 3 <CalGrid [473 × 2]>   
#> 4 Phase 4 <CalGrid [1,273 × 2]> 
#> 5 Phase 5 <CalGrid [1,377 × 2]> 
#> 6 Phase 6 <CalGrid [1,155 × 2]> 
#> 7 Phase 7 <CalGrid [1,327 × 2]> 
```
