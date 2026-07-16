# Radiocarbon dates from Shubayqa 1

Radiocarbon dates from Shubayqa 1, an Epipalaeolithic site in eastern
Jordan, from Richter et al. (2017) .

## Usage

``` r
shub1_radiocarbon
```

## Format

A data frame with 27 rows and 8 variables:

- lab_id:

  character; standardised lab code uniquely identifying the dated
  sample.

- context:

  integer; schematic context number the sample was found in (see
  details).

- phase:

  character; phase the sample was assigned to.

- sample:

  character; description of the sample context, including its real
  context number.

- material:

  character; description of the sample material.

- cra:

  integer; conventional radiocarbon age of the sample in years cal BP.

- error:

  integer; standard error associated with the radiocarbon measurement in
  ± years cal BP.

- outlier:

  logical; whether the sample is considered an outlier.

## Source

Richter et al. (2017)

## Details

`context` refers to the schematic stratigraphy included in the
[shub1](shub1.md) dataset; the real context number is described in
`sample`.

## References

Richter T, Arranz-Otaegui A, Yeomans L, Boaretto E (2017). “High
Resolution AMS Dates from Shubayqa 1, northeast Jordan Reveal Complex
Origins of Late Epipalaeolithic Natufian in the Levant.” *Scientific
reports*, **7**(1), 17025. ISSN 2045-2322.
[doi:10.1038/s41598-017-17096-5](https://doi.org/10.1038/s41598-017-17096-5)
.

## See also

[shub1](shub1.md)
