# Describe dates in CQL

The CQL commands `R_Date`, `F14C_Date`, `C_Date` and `Date` are used to
describe individual dated events in a model. `R_Date` and `F14C_Date`
represent radiocarbon dates expressed in radiocarbon years (the
conventional radiocarbon age, CRA) or *fraction modern* (F14C)
respectively. `C_Date` represents a calendar date. `Date` directly
specifies a date in OxCal's internal format.

## Usage

``` r
cql_r_date(name, date, error)

cql_c_date(name, date, error)

cql_r_f14c(name, date, error)

cql_date(name, date)
```

## Arguments

- name:

  Character. Date label(s), usually a lab code.

- date:

  Numeric. Date or dates expressed in radiocarbon years
  (`cql_r_date()`), F14C (`cql_f14c_date()`), calendar years
  (`cql_c_date()`), or OxCal's internal format (`cql_date()`) See
  details.

- error:

  Integer. Uncertainty associated with the date(s).

## Value

A `cql` object, or a list of `cql` objects if the arguments are vectors.

## Details

The era expected for calendar dates (BP or BC/AD) depends on a global
option in OxCal, which defaults to BC/AD. BC dates are specified as
negative values.

F14C measurements are recommended for modern, "post-bomb" radiocarbon
dates (Reimer et al. 2004) .

OxCal's internal date format, used with `Date()`, is a decimal Gregorian
year, for details see:
<https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_calend.html>

## References

<https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_inform.html#date>

<https://c14.arch.ox.ac.uk/oxcalhelp/hlp_commands.html>

Reimer PJ, Brown TA, Reimer RW (2004). “Discussion: Reporting and
Calibration of Post-Bomb 14C Data.” *Radiocarbon*, **46**(3), 1299–1304.
ISSN 0033-8222, 1945-5755.
[doi:10.1017/S0033822200033154](https://doi.org/10.1017/S0033822200033154)
.

## See also

Other CQL functions: [`cql_boundary()`](cql_boundary.md),
[`cql_n()`](cql_n.md), [`cql_options()`](cql_options.md),
[`cql_other`](cql_other.md), [`cql_phase()`](cql_phase.md),
[`cql_sequence()`](cql_sequence.md), [`cql()`](cql.md),
[`write_oxcal()`](write_oxcal.md)
