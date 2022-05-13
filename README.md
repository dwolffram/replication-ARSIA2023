# Replication package for "Model Diagnostics and Forecast Evaluation for Quantiles"
<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dwolffram/replication-ARSIA2023/main?urlpath=rstudio)
<!-- badges: end -->

Daniel Wolffram, Johannes Resin, Kristof Kraus, Alexander I. Jordan

## Overview & contents

The code in this replication package generates the 10 figures and 1 table of a
review paper on "Model Diagnostics and Forecast Evaluation for Quantiles" using R.
Each figure is generated separately by its corresponding script file
"Figure[xx]*.R". Replication is expected to complete within 1 minute for Figures 1--9,
and within 30 minutes for Figure 10.

The main contents of the repository are the following:

- `data/`: folder of preprocessed data for the COVIDhub and GEFCOM examples
- `R/`: folder of R code with utility and plotting functions
- `figures/`: folder of generated figures in PDF format
- `Figure[xx]*.R`: R script to create the respective figure

A representation of the contents of Table 1 are printed to the console from the
script `Figure09_Table1_reliability_Engel.R`.

## Data availability and provenance

### COVID-19 Forecast Hub

The file `covid19_data_loading.R` collects the data (Cramer et al. 2021) required for the analysis from the GitHub Repository [reichlab/covid19-forecast-hub](https://github.com/reichlab/covid19-forecast-hub) of the COVID-19 Forecast Hub.
This process is expected to complete within 20 minutes, and the result is provided in the `data/` folder.

### Engel data on food expenditure

The data set is loaded from the R package `quantreg` (Koenker 2022) available from CRAN.

### GEFCOM14

The data on the wind power time series is available as Appendix A to (Hong et al. 2016).
After processing and adding predictions from the 3 models described in the paper, the result is provided in the `data/` folder.

## Instructions & computational requirements.

The analysis files `Figure*.R` can be run individually, in any order. Set the working
directory to the root of the replication package, or open the `.Rproj` file
using RStudio.

The software versions that were used to run these analyses are

- R 4.1.3
  - `geomtextpath` (0.1.0)
  - `ggrepel` (0.9.1)
  - `gridExtra` (2.3)
  - `isotone` (1.1-0)
  - `lubridate` (1.8.0)
  - `patchwork` (1.1.1)
  - `quantreg` (5.93)
  - `tidyverse` (1.3.1)
  - and for preprocessing data: `MMWRweek` (0.1.3), `jsonlite` (1.8.0)
  
For a convenient package setup in a (local) R session, use the `checkpoint` package with the command `checkpoint("2022-05-13")` (or source the file `checkpoint.R`) before running any of the analysis scripts.

For a convenient remote setup, follow the `binder` link (https://mybinder.org/v2/gh/dwolffram/replication-ARSIA2023/main?urlpath=rstudio) for an interactive RStudio session in your Browser with access to the replication material.
Depending on the caching status, the setup may take up to 15 minutes.
At the present time (2022-05-13), Figure 10 cannot be reproduced on `binder` due to computational constraints (not enough RAM).

## References

Cramer EY, Huang Y, Wang Y, et al. 2021. The United States COVID-19 Forecast Hub
dataset. _medRxiv_. URL: https://www.medrxiv.org/content/10.1101/2021.11.04.21265886v1

Hong T, Pinson P, Fan S, Zareipour H, Troccoli A, Hyndman RJ. 2016. Probabilistic energy forecasting: Global Energy Forecasting Competition 2014 and beyond. _International Journal of Forecasting, 32(3):896--913. URL: https://doi.org/10.1016/j.ijforecast.2016.02.001

Koenker R. 2022. quantreg: Quantile Regression. R package version 5.88. URL: https://CRAN.R-project.org/package=quantreg
