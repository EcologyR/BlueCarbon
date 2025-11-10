# Estimation of the thickness of the sample

checks for space between samples and, if any, divide this space between
the previous and next sample to return sample thickness without gaps in
the core

## Usage

``` r
estimate_h(
  df = NULL,
  core = "core",
  mind = "mind_corrected",
  maxd = "maxd_corrected"
)
```

## Arguments

- df:

  A data.frame with columns core, mind (minimum depth of the sample) and
  maxd (maximum depth of the sample)

- core:

  Character Name of the column reporting core ID.

- mind:

  Character Name of the column reporting the minimum depth of each
  sample.

- maxd:

  Character Name of the column reporting the maximum depth of each
  sample.

## Value

the initial data.frame with three additional columns:

- emin (estimated minimum depth of the sample)

- emax (estimated maximum depth of the sample)

- h (estimated thickness of the sample)

## Examples

``` r
bluecarbon_decompact <- decompact(bluecarbon_data)
#> Warning: Setting compaction = 0 for these cores: Sm_03_04, Sg_10_02, Sg_11_03, Sm_05_01, Sm_06_01
out <- estimate_h(bluecarbon_decompact)
```
