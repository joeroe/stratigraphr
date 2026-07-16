# Chronological Query Language (CQL)

Provides an R interface for the Chronological Query Language (CQL),
primarily used to input commands and describe models to OxCal.

## Usage

``` r
cql(...)

as_cql(x)

# S3 method for class 'list'
as_cql(x)

# Default S3 method
as_cql(x)
```

## Arguments

- ...:

  `cql` objects to be assembled into a script.

- x:

  Object to be coerced to a `cql` object.

## Value

A CQL script.

## Details

CQL scripts are represented by the S3 class `cql`. `cql()` takes the
output of individual `cql_` functions and assembles them into a single
script. See [`vignette("cql")`](../articles/cql.md) for a tutorial.

List arguments to `cql()` are collapsed to produce a single script. If
you want to coerce an object to `cql` and preserve its structure, use
`as_cql()` instead.

## References

<https://c14.arch.ox.ac.uk/oxcalhelp/hlp_commands.html>

## See also

Other CQL functions: [`cql_boundary()`](cql_boundary.md),
[`cql_n()`](cql_n.md), [`cql_options()`](cql_options.md),
[`cql_other`](cql_other.md), [`cql_phase()`](cql_phase.md),
[`cql_r_date()`](cql_r_date.md), [`cql_sequence()`](cql_sequence.md),
[`write_oxcal()`](write_oxcal.md)
