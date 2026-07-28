# Connect stratigraphic units

Constructs a table of directed edges between stratigraphic units based
on a vector of relations (e.g. "above" or "below").

## Usage

``` r
strat_connect(units, relations, direction = c("above", "below"))
```

## Arguments

- units:

  Vector of unit labels.

- relations:

  Vector of relations.

- direction:

  Are the units in `relations` "above" or "below" the ones in `units`?

## Value

A data frame of directed edges represented by `to` and `from` columns,
which can be used as the `nodes` argument to
[`tidygraph::tbl_graph()`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html).

## Examples

``` r
strat_connect(
  units = c("A", "B", "C"),
  relations = list("B", "C", NA),
  direction = "above"
)
#>   to from
#> 1  A    B
#> 2  B    C
```
