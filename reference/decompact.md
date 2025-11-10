# Calculate sediment properties after decompaction

Accepts a data.frame with sample properties and compaction estimations
and returns a modified version with sample properties corrected for
compaction

## Usage

``` r
decompact(
  df = NULL,
  core = "core",
  compaction = "compaction",
  mind = "mind",
  maxd = "maxd",
  dbd = NULL
)
```

## Arguments

- df:

  Data.frame with core properties

- core:

  Character Name of the column with the id of the core to which the
  sample belongs

- compaction:

  Character Name of the column with core compaction IN PERCENTAGE, as
  calculated with
  [`estimate_compaction()`](https://ecologyr.github.io/BlueCarbon/reference/estimate_compaction.md).

- mind:

  Character Name of the column with minimum depth of the sample (depth
  at the top of the sample)

- maxd:

  Character Name of the column with maximum depth of the sample (depth
  at the bottom of the sample)

- dbd:

  Character Name of the column with dry bulk density

## Value

The initial data.frame with the addition of two columns with the
corrected minimum and maximum depth of the samples (additionally, if a
dry bulk density column is specified, it will return another column with
corrected dry bulk density)

## Examples

``` r
decompact(bluecarbon_data) |>
  head()
#> Warning: Setting compaction = 0 for these cores: Sm_03_04, Sg_10_02, Sg_11_03, Sm_05_01, Sm_06_01
#>    site     core ecosystem            species compaction mind maxd       dbd
#> 1 Sg_01 Sg_01_01  Seagrass Posidonia oceanica   5.714286    0    1 0.7352912
#> 2 Sg_01 Sg_01_01  Seagrass Posidonia oceanica   5.714286    1    2 0.9754336
#> 3 Sg_01 Sg_01_01  Seagrass Posidonia oceanica   5.714286    2    3 0.8698411
#> 4 Sg_01 Sg_01_01  Seagrass Posidonia oceanica   5.714286    3    4 1.0272564
#> 5 Sg_01 Sg_01_01  Seagrass Posidonia oceanica   5.714286    4    5 0.9307887
#> 6 Sg_01 Sg_01_01  Seagrass Posidonia oceanica   5.714286    5    6 1.4696196
#>         om oc age mind_corrected maxd_corrected
#> 1 6.554329 NA   8       0.000000       1.060606
#> 2       NA NA  13       1.060606       2.121212
#> 3 7.382634 NA  15       2.121212       3.181818
#> 4       NA NA  22       3.181818       4.242424
#> 5 8.026646 NA  29       4.242424       5.303030
#> 6       NA NA  35       5.303030       6.363636
```
