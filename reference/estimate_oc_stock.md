# Organic Carbon Stock estimation

Estimates carbon stocks from soil core data down to a specified depth,
100 cm by default. If the core does not reach the standardized depth, it
extrapolates the stock from a linear model between accumulated mass of
organic carbon and depth.

## Usage

``` r
estimate_oc_stock(
  df = NULL,
  depth = 100,
  core = "core",
  mind = "mind_corrected",
  maxd = "maxd_corrected",
  dbd = "dbd",
  oc = "eoc"
)
```

## Arguments

- df:

  A data.frame with core (core id), mind (minimum depth of the sample),
  maxd (maximum depth of the sample), dbd (dry bulk density), oc
  (organic carbon %)

- depth:

  max depth to estimate the stock, by default 100.

- core:

  Character Name of the column reporting core ID.

- mind:

  Character Name of the column reporting the minimum depth of each
  sample.

- maxd:

  Character Name of the column reporting the maximum depth of each
  sample.

- dbd:

  Character Name of the column reporting dry bulk density.

- oc:

  Character Name of the column reporting organic carbon concentrations.

## Value

data.frame with columns core, stockwc (organic carbon stock at the whole
core), maxd (maximum depth of the core), stock (organic carbon stock at
the standardized depth), and stock_se (standard error for the estimated
stock).

## Examples

``` r
bluecarbon_decompact <- decompact(bluecarbon_data)
#> Warning: Setting compaction = 0 for these cores: Sm_03_04, Sg_10_02, Sg_11_03, Sm_05_01, Sm_06_01

oc <- estimate_oc(bluecarbon_decompact)

#> Warning: The following cores had samples with organic carbon values below the organic carbon range used to built the model: Sg_04_01, Sm_04_03, Sm_04_04, Sm_05_01
#> Warning: The following cores had samples with organic carbon values above the organic carbon range used to built the model: Sg_04_01, Sm_03_01, Sm_04_02, Sm_04_03, Sm_04_04, Sm_05_01

out <- estimate_oc_stock(oc[[1]])
head(out)
#>       core    stockwc      maxd     stock   stock_se
#> 1 Sg_01_01 2.62947377 156.96970 1.8551331         NA
#> 2 Sg_01_02 2.37997169  78.34839 2.9938578 0.09274054
#> 3 Sg_01_03 2.08002346  99.63509 2.1763784 0.06938244
#> 4 Sg_02_01 2.06631095 124.21053 1.6774846         NA
#> 5 Sg_02_02 0.06798487  23.14286 0.3099306 0.01537332
#> 6 Sg_02_03 0.93076430  55.82951 1.6394040 0.05576503
```
