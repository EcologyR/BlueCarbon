# Estimate the average organic carbon sequestration rate

estimate the average organic carbon sequestration rate to the soil in a
indicated time frame (by default last 100 years) from the organic carbon
concentration and ages obtained from a age-depth or age-accumulated mass
model

## Usage

``` r
estimate_seq_rate(
  df = NULL,
  timeframe = 100,
  core = "core",
  mind = "mind_corrected",
  maxd = "maxd_corrected",
  dbd = "dbd",
  oc = "eoc",
  age = "age"
)
```

## Arguments

- df:

  A data.frame with, at least, columns: core, mind (minimum depth of the
  sample), maxd (maximum depth of the sample), dbd (dry bulk density),
  oc (organic carbon %), age (age of the sample obtained from a
  age-depth or age-accumulated mass model)

- timeframe:

  standardization time frame, by default 100 years

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

- age:

  Character Name of the column reporting the age of each sample.

## Value

data.frame with columns 'core', seq_rate_wc (organic carbon
sequestration rates at the whole core), maxage (maximum age of the
core), and seq_rate (average organic carbon sequestration rate at the
indicated time frame)

## Examples

``` r
bluecarbon_decompact <- decompact(bluecarbon_data)
#> Warning: Setting compaction = 0 for these cores: Sm_03_04, Sg_10_02, Sg_11_03, Sm_05_01, Sm_06_01
oc <- estimate_oc(bluecarbon_decompact)

#> Warning: The following cores had samples with organic carbon values below the organic carbon range used to built the model: Sg_04_01, Sm_04_03, Sm_04_04, Sm_05_01
#> Warning: The following cores had samples with organic carbon values above the organic carbon range used to built the model: Sg_04_01, Sm_03_01, Sm_04_02, Sm_04_03, Sm_04_04, Sm_05_01
out <- estimate_seq_rate(oc[[1]])
head(out)
#>       core  seq_rate_wc maxage     seq_rate
#> 1 Sg_01_01 0.0020180152   1303 0.0043626875
#> 2 Sg_02_01 0.0005423388   3810 0.0027556600
#> 3 Sg_03_01 0.0001068260   1618 0.0002322665
#> 4 Sg_05_01 0.0023649409    422 0.0022770729
#> 5 Mg_01_01 0.0024467131    990 0.0040171986
#> 6 Sm_01_01 0.0009239537   1247 0.0012018703
```
