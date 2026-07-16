# Calibrate radiocarbon dates

A thin wrapper of
[`rcarbon::calibrate()`](https://rdrr.io/pkg/rcarbon/man/calibrate.html)
that returns calibrated dates as a single list rather than a \`CalDates“
object. Useful, for example, if you want to add a column of calibrated
dates to an existing table with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

## Usage

``` r
c14_calibrate(cra, error, ..., engine = c("rcarbon", "OxCal", "Bchron"))
```

## Arguments

- cra:

  A vector of uncalibrated radiocarbon ages.

- error:

  A vector of standard errors associated with `cra`

- ...:

  Optional arguments passed to calibration function.

- engine:

  Package to use for calibration, i.e.
  [`rcarbon::calibrate()`](https://rdrr.io/pkg/rcarbon/man/calibrate.html)
  (`"rcarbon"`),
  [`oxcAAR::oxcalCalibrate()`](https://rdrr.io/pkg/oxcAAR/man/oxcalCalibrate.html)
  (`"OxCal"`), or
  [`Bchron::BchronCalibrate()`](https://andrewcparnell.github.io/Bchron/reference/BchronCalibrate.html)
  (`"Bchron"`). Default: `"rcarbon"`.

## Value

A list of `cal` objects.

## See also

Other tidy radiocarbon functions: [`c14_sum()`](c14_sum.md),
[`cal()`](cal.md)

## Examples

``` r
data("shub1_radiocarbon")
shub1_radiocarbon %>%
  dplyr::mutate(cal = c14_calibrate(cra, error))
#> [1] "Calibrating radiocarbon ages..."
#>   |                                                                              |                                                                      |   0%  |                                                                              |===                                                                   |   4%  |                                                                              |=====                                                                 |   7%  |                                                                              |========                                                              |  11%  |                                                                              |==========                                                            |  15%  |                                                                              |=============                                                         |  19%  |                                                                              |================                                                      |  22%  |                                                                              |==================                                                    |  26%  |                                                                              |=====================                                                 |  30%  |                                                                              |=======================                                               |  33%  |                                                                              |==========================                                            |  37%  |                                                                              |=============================                                         |  41%  |                                                                              |===============================                                       |  44%  |                                                                              |==================================                                    |  48%  |                                                                              |====================================                                  |  52%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  59%  |                                                                              |============================================                          |  63%  |                                                                              |===============================================                       |  67%  |                                                                              |=================================================                     |  70%  |                                                                              |====================================================                  |  74%  |                                                                              |======================================================                |  78%  |                                                                              |=========================================================             |  81%  |                                                                              |============================================================          |  85%  |                                                                              |==============================================================        |  89%  |                                                                              |=================================================================     |  93%  |                                                                              |===================================================================   |  96%  |                                                                              |======================================================================| 100%
#> [1] "Done."
#> Warning: There were 2 warnings in `dplyr::mutate()`.
#> The first warning was:
#> ℹ In argument: `cal = c14_calibrate(cra, error)`.
#> Caused by warning:
#> ! stratigraphr::c14_calibrate() is deprecated. Please use c14::c14_calibrate() instead.
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
#> # A tibble: 27 × 9
#>    lab_id      context phase   sample         material   cra error outlier cal  
#>    <chr>         <int> <chr>   <chr>          <chr>    <int> <int> <lgl>   <lis>
#>  1 RTD-7951         23 Phase 7 Context 126, … Bolbosc… 12166    55 FALSE   <cal>
#>  2 Beta-112146      24 Phase 7 SHUB1/105      gazelle… 12310    60 FALSE   <cal>
#>  3 RTD-7317         26 Phase 7 Context 83, S… Bolbosc… 12289    46 FALSE   <cal>
#>  4 RTD-7318         27 Phase 7 Context 86, S… Zilla s… 12332    46 FALSE   <cal>
#>  5 RTD-7948         24 Phase 7 Context 120, … Bolbosc… 12478    38 FALSE   <cal>
#>  6 RTD-7947         22 Phase 6 Context 166, … Bolbosc… 12322    38 FALSE   <cal>
#>  7 RTD-7313         22 Phase 6 Context 67, K… Bolbosc… 12346    46 FALSE   <cal>
#>  8 RTD-7311         22 Phase 6 Context 67, K… Vitex s… 12367    65 FALSE   <cal>
#>  9 RTD-7312         22 Phase 6 Context 67, K… Zilla s… 12405    50 FALSE   <cal>
#> 10 RTD-7314         22 Phase 6 Context 67, K… Bolbosc… 12273    48 FALSE   <cal>
#> # ℹ 17 more rows
```
