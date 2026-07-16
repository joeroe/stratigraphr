# Set OxCal options in CQL

This function generates a CQL command that sets OxCal's global options.

## Usage

``` r
cql_options(
  bcad = TRUE,
  convergence_data = FALSE,
  curve = "intcal20.14c",
  cubic = TRUE,
  ensembles = 30,
  floruit = FALSE,
  intercept = FALSE,
  k_iterations = 30,
  plus_minus = FALSE,
  raw_data = FALSE,
  resolution = 5,
  round = FALSE,
  round_by = 0,
  sd1 = TRUE,
  sd2 = TRUE,
  sd3 = FALSE,
  uniform_span_prior = TRUE,
  use_f14c = TRUE,
  year = 1950.5,
  ...
)
```

## Arguments

- bcad:

  Logical. Whether BC/AD are used in the log file output.

- convergence_data:

  Logical. Whether sample convergence data is included in the output
  data file.

- curve:

  Character. The default calibration curve.

- cubic:

  Logical. Whether cubic (as opposed to linear) interpolation is used
  for calibration curves.

- ensembles:

  Integer. The number of age-depth ensembles stored during the analysis.

- floruit:

  Logical. Whether quantile ranges are calculated instead of the default
  highest posterior density (hpd).

- intercept:

  Logical. Whether the intercept method is used for radiocarbon
  calibration ranges.

- k_iterations:

  Integer. The default number of MCMC passes.

- plus_minus:

  Logical. Whether + and - are used in place of BC and AD in log files.

- raw_data:

  Logical. Whether raw calibration curve data is included in the output
  data file.

- resolution:

  Integer. The default bin size for probability distributions of Date
  and Interval type.

- round:

  Logical. Whether ranges are rounded off.

- round_by:

  Integer. Resolution of rounding (0 for automatic).

- sd1:

  Logical. Whether 68.2% (1 σ) ranges are given in the log and tab
  delimited files.

- sd2:

  Logical. Whether 95.4% (2 σ) ranges are given in the log and tab
  delimited files.

- sd3:

  Logical. Whether 99.7% (3 σ) ranges are given in the log and tab
  delimited files.

- uniform_span_prior:

  Logical. Whether the two extra prior factors suggested by Nicholls and
  Jones 2001 are used.

- use_f14c:

  Logical. Whether all calibrations take place in F14C space (rather
  than BP space).

- year:

  Numeric. The datum point for ages - the default is mid AD 1950.

- ...:

  Additional named arguments converted to OxCal options. See details.

## Value

A `cql` object.

## Details

Parameter descriptions and defaults are taken from the OxCal v4.4
documentation (see references).

Options not explicitly included here can be specified as additional
named arguments to this function. The name must exactly match the name
of the option expected by OxCal; unknown or misspelled options are
silently ignored.

## References

<https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_detail.html#opt>

## See also

Other CQL functions: [`cql_boundary()`](cql_boundary.md),
[`cql_n()`](cql_n.md), [`cql_other`](cql_other.md),
[`cql_phase()`](cql_phase.md), [`cql_r_date()`](cql_r_date.md),
[`cql_sequence()`](cql_sequence.md), [`cql()`](cql.md),
[`write_oxcal()`](write_oxcal.md)
