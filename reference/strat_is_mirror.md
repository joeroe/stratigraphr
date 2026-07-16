# Are two relation vectors mirrored?

Checks whether one vector of relations is the inverse of another.
Typically used to confirm that "above" and "below" columns match and
will result in the same stratigraphic graph.

## Usage

``` r
strat_is_mirror(units, relation1, relation2)
```

## Arguments

- units:

  Vector of unit labels.

- relation1:

  First vector of relations.

- relation2:

  Second vector of relations.

## Value

`TRUE` or `FALSE`

## Examples

``` r
data("harris12")
strat_is_mirror(harris12$context, harris12$above, harris12$below)
#> [1] TRUE
```
