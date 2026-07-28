# Write CQL to a file

Writes a CQL script (from
[`cql()`](https://stratigraphr.joeroe.io/reference/cql.md)) to an .oxcal
file, for input to OxCal.

## Usage

``` r
write_oxcal(cql, file)
```

## Arguments

- cql:

  A `cql` object. See
  [`cql()`](https://stratigraphr.joeroe.io/reference/cql.md).

- file:

  Path to a file.

## Value

Returns `cql` invisibly.

## See also

Other CQL functions:
[`cql()`](https://stratigraphr.joeroe.io/reference/cql.md),
[`cql_boundary()`](https://stratigraphr.joeroe.io/reference/cql_boundary.md),
[`cql_n()`](https://stratigraphr.joeroe.io/reference/cql_n.md),
[`cql_options()`](https://stratigraphr.joeroe.io/reference/cql_options.md),
[`cql_other`](https://stratigraphr.joeroe.io/reference/cql_other.md),
[`cql_phase()`](https://stratigraphr.joeroe.io/reference/cql_phase.md),
[`cql_r_date()`](https://stratigraphr.joeroe.io/reference/cql_r_date.md),
[`cql_sequence()`](https://stratigraphr.joeroe.io/reference/cql_sequence.md)

## Examples

``` r
if (FALSE) { # \dontrun{
script <- cql(
  cql_r_date("ABC-001", 10100, 50),
  cql_r_date("ABC-002", 10200, 50)
)
write_oxcal(script, tempfile(fileext = ".oxcal"))
} # }
```
