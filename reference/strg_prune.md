# Remove redundant relations from a stratigraph

strg_prune() removes redundant relations from a stratigraphic graph by
computing its [transitive
reduction](https://en.wikipedia.org/wiki/Transitive_reduction). The
result is a 'pruned' graph that follows Harris' "Law of Stratigraphical
Succession": that only the uppermost and undermost relations are
significant when placing a unit in a stratigraphic sequence.

## Usage

``` r
strg_prune(strg)
```

## Arguments

- strg:

  A
  [`stratigraph()`](https://stratigraphr.joeroe.io/reference/stratigraph.md)
  object to prune.

## Value

A [stratigraph](https://stratigraphr.joeroe.io/reference/stratigraph.md)
with redundant relations removed.

## Examples

``` r

bushy_stratigraphy <- stratigraph(
  tibble::tibble(
    id = letters[1:5],
    above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
  ),
  "id", "above"
)
#> Warning: Invalid stratigraphic graph:
#> ! Contains redundant relations
#> ℹ Use `strg_prune()` to remove redundant relations

strg_prune(bushy_stratigraphy)
#> # A stratigraph: 5 units and 5 relations
#> # ✔ Valid stratigraphic graph
#>   a
#> ┌─┴─┐
#> b   c
#> └─┬─┘
#>   d
#>   │
#>   e
```
