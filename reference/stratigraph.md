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
  column and a column describing stratigraphic relations. The relation
  column can be either a list column (where each element is a vector of
  related units) or a regular column (where each row represents a single
  relation). If the relation column is a list, it will be automatically
  unnested to long format.

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
# Long format (one relation per row)
stratigraph(data.frame(
  label = c("A", "B", "C", "C"),
  below = c("B", "C", "A", "B")
), "label", "below")
#> Warning: Invalid stratigraphic graph:
#> ! Contains cycles
#> # A stratigraph: 3 units and 4 relations
#> # ✖ Invalid stratigraphic graph
#> 
#> ! Contains cycles
```
