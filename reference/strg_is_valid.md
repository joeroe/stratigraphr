# Is an object a valid stratigraphic graph?

Is an object a valid stratigraphic graph?

## Usage

``` r
strg_is_valid(stratigraph, warn = TRUE)
```

## Arguments

- stratigraph:

  A `stratigraph` object (see [`stratigraph()`](stratigraph.md)).

- warn:

  Display warnings for invalid graphs? Default: `TRUE`.

## Value

`TRUE` or `FALSE`.

## Details

Checks whether a stratigraph object is a valid stratigraphic graph.
Currently looks for:

- Whether the graph contains cycles
