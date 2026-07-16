# Schematic stratigraphy of Shubayqa 1

A simplified version of the stratigraphy of Shubayqa 1, an
Epipalaeolithic site in eastern Jordan, after Richter et al. (2017) .

## Usage

``` r
shub1
```

## Format

A data frame with 30 rows, representing contexts, and 7 variables:

- context:

  integer; a unique identifier of the context.

- type:

  character; type of context, i.e. deposit, fill, cut, or structural.

- above:

  integer vector; context(s) stratigraphically above this one.

- below:

  integer vector; context(s) stratigraphically below this one.

- equal:

  integer vector; context(s) stratigraphically equal to this one.

- phase:

  character; Phase assigned to the context, for contexts that aren't
  structural.

- structure:

  character; for structural contexts, the name of the structure they
  belong to.

## Source

Richter et al. (2017)

## Details

The stratigraphy is a simplified version derived from the schematic
section in Richter et al. (2017) , figure 2. Context numbers were
arbitrarily assigned and the stratigraphic relations are based on those
evident in the diagram. Phase and structure names are also based on the
diagram.

## References

Richter T, Arranz-Otaegui A, Yeomans L, Boaretto E (2017). “High
Resolution AMS Dates from Shubayqa 1, northeast Jordan Reveal Complex
Origins of Late Epipalaeolithic Natufian in the Levant.” *Scientific
reports*, **7**(1), 17025. ISSN 2045-2322.
[doi:10.1038/s41598-017-17096-5](https://doi.org/10.1038/s41598-017-17096-5)
.

## See also

[shub1_radiocarbon](shub1_radiocarbon.md)
