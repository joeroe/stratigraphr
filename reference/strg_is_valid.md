# Validation of stratigraphic graphs

Validation of stratigraphic graphs

## Usage

``` r
strg_is_valid(stratigraph)

strg_validate(stratigraph, warn = FALSE)
```

## Arguments

- stratigraph:

  A `stratigraph` object (see
  [`stratigraph()`](https://stratigraphr.joeroe.io/reference/stratigraph.md)).

- warn:

  If `TRUE`, signal warnings instead of errors. Default: `FALSE`.

## Value

`strg_is_valid()` returns `TRUE` or `FALSE`. `strg_validate()` returns
the object (or signals an error/warning if invalid).

## Details

Checks whether a stratigraph object is a valid stratigraphic graph.
Currently looks for:

- Whether the graph contains cycles (violating the law of stratigraphic
  superposition)

- Whether the graph contains redundant relations (i.e. relations that
  are implied by transitivity)

`strg_is_valid()` returns `TRUE` or `FALSE` silently. `strg_validate()`
signals an error (or warning if `warn = TRUE`) with details of any
validity issues found.

## Examples

``` r
data("harris12")
strg <- stratigraph(harris12, "context", "above")

strg_is_valid(strg)
#> [1] TRUE
strg_validate(strg)
#> # A stratigraph: 10 units and 12 relations
#> # ✔ Valid stratigraphic graph
#>                 1
#> ┌───────────────┼───────────────┐
#> 2               3               4
#> └───────────────┼───────────────┘
#>                 5
#>                 │
#>                 6
#>         ┌───────┴───────┐
#>         7               8
#>         └───────┬───────┘
#>                 9
#>                 │
#>              natural
```
