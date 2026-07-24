# Construct a stratigraphic graph

Takes a data frame describing stratigraphic units and relations and
turns it into a graph representation.

## Usage

``` r
stratigraph(data, label, relation, direction = c("above", "below"))
```

## Arguments

- data:

  Data frame of stratigraphic units, containing at least a unique label
  column and a column describing stratigraphic relations.

- label:

  Name of the column containing labels of the stratigraphic units.

- relation:

  Name of the column describing the stratigraphic relations between
  units.

- direction:

  Direction described by `relation`, i.e. are the units in that column
  "above" or "below" the ones in units. Default: "above".

## Value

A `stratigraph` object.

## Examples

``` r
circle <- stratigraph(data.frame(
  label = LETTERS[1:4],
  below = c("B", "C", "D", "A")
), "label", "below", "below")
#> Warning: Invalid stratigraphic graph: contains cycles
```
