# Estimate Core Compaction

Estimates the percentage of core compaction using measurements from a
data.frame containing core properties. It computes a correction factor
based on sampler tube length, internal distance, and external distance,
and adds a 'compaction' column to the input data.frame with the
calculated compaction rate as a percentage.

## Usage

``` r
estimate_compaction(
  df = NULL,
  core = "core",
  sampler_length = "sampler_length",
  internal_distance = "internal_distance",
  external_distance = "external_distance"
)
```

## Arguments

- df:

  A data.frame containing core properties.

- core:

  Character Name of the column identifying each core.

- sampler_length:

  Character Name of the column with the total length of the sampler
  tube.

- internal_distance:

  Character Name of the column with the distance between sampler top and
  core surface.

- external_distance:

  Character Name of the column with the distance between sampler top and
  sediment surface.

## Value

Returns the input data.frame with an additional 'compaction' column
indicating the estimated percentage of core compaction.

## Examples

``` r
df <- estimate_compaction(
  core_comp,
  core = "core",
  sampler_length = "sampler_length",
  internal_distance = "internal_distance",
  external_distance = "external_distance"
)
#> Warning: Removing cores with missing data: Sm_03_04
head(df)
#>       core sampler_length internal_distance external_distance compaction
#> 1 Sg_01_01            200                35                25   5.714286
#> 2 Sg_01_02            200                45                35   6.060606
#> 3 Sg_01_03            200                86                76   8.064516
#> 4 Sg_02_01            200                10                 0   5.000000
#> 5 Sg_02_02            200                60                50   6.666667
#> 6 Sg_02_03            200                78                68   7.575758

```
