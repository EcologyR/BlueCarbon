
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BlueCarbon

<!-- badges: start -->

![](https://img.shields.io/github/r-package/v/EcologyR/BlueCarbon)
[![R-CMD-check](https://github.com/EcologyR/BlueCarbon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EcologyR/BlueCarbon/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/EcologyR/BlueCarbon/graph/badge.svg?token=5PabVL1UJK)](https://codecov.io/gh/EcologyR/BlueCarbon)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of BlueCarbon is to facilitate the estimation of organic carbon
stocks and sequestration rates from soil/sediment cores from blue carbon
ecosystems. It contains seven main functions to estimate the compaction
of cores, mathematically correct core compaction, estimate sample
thickness, estimate organic carbon content from organic matter content,
estimate organic carbon stocks and sequestration rates and visualize the
error of stock extrapolation.

<figure>
<img src="man/figures/BC_workflow-01.png"
alt="Blue Carbon library workflow" />
<figcaption aria-hidden="true">Blue Carbon library workflow</figcaption>
</figure>

#### ***estimate_compaction*** **- Estimate Core Compaction**

Sampling soil cores by manual percussion usually leads to the compaction
of the material retrieved. This function will estimate the percentage of
compaction from measurements taken in the field after inserting the
corer tube and before extracting it: length of the corer tube, distance
between the surface of the soil and the top of the tube in the outside
and distance between the surface of the soil and the top of the tube in
the inside of the tube.

#### ***decompact*** **- Calculate sediment properties after decompaction**

Core compaction derived from field extraction can be mathematically
correct to estimate the original depth of the samples. This function
will apply a linear correction (all the core material is assumed to have
been compacted equally) to correct sample depth and, if provided, dry
bulk density.

#### ***estimate_oc*** **- Organic carbon content estimation from organic carbon data**

There is linear correlation between organic carbon and organic matter
content. This correlation can change between ecosystems and sampling
sites due to changes in organic matter composition among other factors.
This function model a linear correlation between organic matter and
organic carbon content in your samples and predict the content of
organic carbon for those samples were there is no organic carbon values.
Estimation of organic carbon is done by means of linear regressions on
log(organic carbon) ~ log(organic matter). It gives back a organic
carbon value for each organic matter value provided. If there is a
organic carbon value for that sample it return the same value, else,
generates a model for that site, else, model for specie, else, model for
that ecosystem. If a model can not be created due to the low number of
samples (\<10) it uses the equations in Fourqurean et al. 2012 for
seagrasses, Maxwell et al. 2023 for salt marshes and Piñeiro-Juncal in
prepp for mangroves to estimate the organic carbon. It is unlikely, but
possible, that a model will predict a higher organic carbon than organic
matter content. This is not possible in nature. If this is the case the
function will give a warning and it is recommended to discard that
model.

Fourqureanet al. (2012) Seagrass ecosystems as a globally significant
carbon stock. Nat. Geosci.5, 505–509. <https://doi.org/10.1038/ngeo1477>

Maxwell et al. (2023) Global dataset of soil organic carbon in tidal
marshes.Sci.Data 10, 1–14.<https://doi.org/10.1038/s41597-023-02633-x>

Piñeiro-Juncal et al. (in prepp) Soil organic carbon preservation and
decay trends in tidal marsh, mangrove and seagrass blue carbon
ecosystems.

#### ***estimate_h*** **- Sample thickness estimation**

For those cores were only selected samples were measured it is necessary
to assign a carbon density to the empty spaces before the estimation the
total stock. This function checks for gaps between samples and, if any,
divide this space between the previous and next sample to return sample
thickness without gaps in the core. The stock and sequestration rate
estimation functions (estimate_oc_stock and estimate_seq_rate) have this
function incorporated and it is not necessary to run it beforehand.

#### ***estimate_oc_stock*** **- Organic carbon stock estimation**

Estimates carbon stocks from soil core data down to a specified depth,
100 cm by default. If the core does not reach the desired depth, it
extrapolates the stock from a linear model between accumulated mass of
organic carbon and depth.

#### ***test_extrapolation*** **- Visualize the error of stock extrapolation**

This function subset those cores that reach the desired depth, estimates
the stock (observed stock), estimate the stock from the linear relation
of organic carbon accumulated mass and depth using the 90, 75, 50 and
25% top length of the indicated desired depth. Compares the observed
stock with the estimated stocks by extrapolation. This function requires
that some of you core do reach the desired depth.

#### ***estimate_seq_rate*** **- Organic carbon sequestration rates estimation**

Estimate the average organic carbon sequestration rate to the soil in a
indicated time frame (by default last 100 years) from the organic carbon
concentration and the age of the samples.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("EcologyR/BlueCarbon")
```

The code to create this package is available
[here](https://github.com/EcologyR/BlueCarbon).

## Citation

If using this package, please cite it:

``` r
citation("BlueCarbon")
To cite package 'BlueCarbon' in publications use:

  Piñeiro-Juncal N, Astigarraga J, Costa V, Martins M,
  Rodriguez-Sanchez F (2024). _BlueCarbon: Estimation of Organic Carbon
  Stocks and Fluxes From Soil Core Data_. R package version 0.0.1,
  https://EcologyR.github.io/BlueCarbon/,
  <https://github.com/EcologyR/BlueCarbon>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {BlueCarbon: Estimation of Organic Carbon Stocks and Fluxes From Soil Core Data},
    author = {Nerea Piñeiro-Juncal and Julen Astigarraga and Valentina Costa and Marcio Martins and Francisco Rodriguez-Sanchez},
    year = {2024},
    url = {https://github.com/EcologyR/BlueCarbon},
    note = {R package version 0.0.1, https://EcologyR.github.io/BlueCarbon/},
  }
```

## Funding

The development of this software has been funded by Fondo Europeo de
Desarrollo Regional (FEDER) and Consejería de Transformación Económica,
Industria, Conocimiento y Universidades of Junta de Andalucía (proyecto
US-1381388 led by Francisco Rodríguez Sánchez, Universidad de Sevilla).

![](https://ecologyr.github.io/workshop/images/logos.png)
