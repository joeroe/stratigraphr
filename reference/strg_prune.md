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

  A [`stratigraph()`](stratigraph.md) object to prune.

## Value

A [stratigraph](stratigraph.md) with redundant relations removed.

## Examples

``` r

bushy_stratigraphy <- stratigraph(
  tibble::tibble(
    id = letters[1:5],
    above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
  ),
  "id", "above"
)

strg_prune(bushy_stratigraphy)
#> # A tbl_graph: 5 nodes and 5 edges
#> #
#> # A directed acyclic simple graph with 1 component
#> #
#> # Node Data: 5 × 2 (active)
#>   id    above    
#>   <chr> <list>   
#> 1 a     <lgl [1]>
#> 2 b     <chr [1]>
#> 3 c     <chr [1]>
#> 4 d     <chr [3]>
#> 5 e     <chr [3]>
#> #
#> # Edge Data: 5 × 2
#>    from    to
#>   <int> <int>
#> 1     1     2
#> 2     1     3
#> 3     2     4
#> # ℹ 2 more rows
```
