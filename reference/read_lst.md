# Read an LST file

Reads stratigraphic data in LST format (used by BASP Harris, Stratify,
and ArchEd) into a data frame.

## Usage

``` r
read_lst(file, split = TRUE, sep = ",", locale = vroom::default_locale())
```

## Arguments

- file:

  Path to an LST file.

- split:

  Controls how attributes that contain multiple values are handled. If
  `TRUE`, all attributes except `name` will be split if their values
  contain the character specified by `sep`. If `FALSE`, no splitting
  will be done. Alternatively, specify a vector of column names
  (corresponding to LST attribute names, see details) that should be
  split.

- sep:

  Delimiter used to separate multiple values in attributes. multiple
  values. Ignored if `split = FALSE`. Default: `","`.

- locale:

  [`vroom::locale()`](https://vroom.tidyverse.org/reference/locale.html)
  object specifying the character encoding and other region-specific
  settings used in the file. Defaults to
  [`vroom::default_locale()`](https://vroom.tidyverse.org/reference/locale.html).

## Value

A data frame. Each row represents one stratum. Columns contain the name
of the stratum and the attributes associated with it (typically "above",
"contemporary_with", "equal_to" and "below").

Attribute names are standardised when read into column names.
Specifically, they are transformed to lower case and spaces are replaced
with an underscore (\_).

Splitting multiple values into vectors is useful if the output is to be
used to construct a stratigraphic graph with
[`stratigraph()`](https://stratigraphr.joeroe.io/reference/stratigraph.md),
but will convert affected columns from atomic vectors to lists, which
can make them awkward to work with in other contexts. Set
`split = FALSE` to avoid this. You can manually split them later with
[`stringr::str_split()`](https://stringr.tidyverse.org/reference/str_split.html).

## Details

This function supports both the original LST format used by BASP Harris
and the "extended" format used by ArchEd and Stratify.

## References

<http://archaeologic.al/wiki/Harris_Matrix#LST>

## Examples

``` r
# Example data from https://github.com/lparchaeology/harris2graph
# Simple LST (BASP Harris)
basp_lst <- system.file("extdata", "bonn.lst", package = "stratigraphr")
read_lst(basp_lst)
#> # A tibble: 19 × 5
#>    name  above     contemporary_with equal_to below    
#>    <chr> <list>    <chr>             <chr>    <list>   
#>  1 +     <chr [1]> NA                NA       <chr [4]>
#>  2 -     <chr [5]> NA                NA       <chr [1]>
#>  3 1     <chr [1]> NA                NA       <chr [1]>
#>  4 2     <chr [1]> NA                NA       <chr [2]>
#>  5 27    <chr [1]> NA                NA       <chr [2]>
#>  6 28    <chr [1]> NA                NA       <chr [1]>
#>  7 29    <chr [1]> NA                NA       <chr [1]>
#>  8 3     <chr [1]> NA                NA       <chr [1]>
#>  9 30    <chr [2]> NA                NA       <chr [1]>
#> 10 35    <chr [2]> NA                NA       <chr [1]>
#> 11 4     <chr [1]> NA                NA       <chr [1]>
#> 12 41    <chr [2]> NA                NA       <chr [1]>
#> 13 42    <chr [1]> NA                NA       <chr [1]>
#> 14 43    <chr [1]> NA                NA       <chr [1]>
#> 15 44    <chr [1]> NA                NA       <chr [1]>
#> 16 52    <chr [1]> NA                NA       <chr [4]>
#> 17 53    <chr [1]> NA                NA       <chr [1]>
#> 18 54    <chr [1]> NA                NA       <chr [1]>
#> 19 70    <chr [2]> NA                NA       <chr [1]>

# Extended LST (Stratify, ArchEd)
stratify_lst <- system.file(
  "extdata", "stratify.lst", package = "stratigraphr"
)
read_lst(stratify_lst)
#> # A tibble: 17 × 4
#>    name    below     unit_class above    
#>    <chr>   <list>    <chr>      <list>   
#>  1 TST::1  <chr [1]> context    <chr [1]>
#>  2 TST::2  <chr [2]> context    <chr [1]>
#>  3 TST::27 <chr [2]> context    <chr [1]>
#>  4 TST::28 <chr [1]> context    <chr [1]>
#>  5 TST::29 <chr [1]> context    <chr [1]>
#>  6 TST::3  <chr [1]> context    <chr [1]>
#>  7 TST::30 <chr [1]> context    <chr [2]>
#>  8 TST::35 <chr [1]> context    <chr [2]>
#>  9 TST::4  <chr [1]> context    <chr [1]>
#> 10 TST::41 <chr [1]> context    <chr [2]>
#> 11 TST::42 <chr [1]> context    <chr [1]>
#> 12 TST::43 <chr [1]> context    <chr [1]>
#> 13 TST::44 <chr [1]> context    <chr [1]>
#> 14 TST::52 <chr [4]> context    <chr [1]>
#> 15 TST::53 <chr [1]> context    <chr [1]>
#> 16 TST::54 <chr [1]> context    <chr [1]>
#> 17 TST::70 <chr [1]> context    <chr [2]>
```
