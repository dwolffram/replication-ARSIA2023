# Replication package for "Model Diagnostics and Forecast Evaluation for Quantiles"
<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dwolffram/replication-ARSIA2023/main?urlpath=rstudio)
<!-- badges: end -->

Daniel Wolffram, Johannes Resin, Kristof Kraus

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

The file "covid19_data_loading.R" collects the data (Cramer et al. 2021) required for the analysis from
the GitHub Repository [reichlab/covid19-forecast-hub](https://github.com/reichlab/covid19-forecast-hub) of the COVID-19 Forecast Hub. This process is expected to
complete within 20 minutes, and the result is provided in the `data/` folder.

### Engel data on food expenditure

The data set is loaded from the R package `quantreg` (Koenker 2022) on CRAN.

### GEFCOM14

## Instructions & computational requirements.

The analysis files `Figure*.R` can be run individually, in any order. Set the working
directory to the root of the replication package, or open the `.Rproj` file
using RStudio.

The software versions that were used to run these analysis are

- R 4.1.3
  - `geomtextpath` (0.1.0)
  - `ggrepel` (0.9.1)
  - `gridExtra` (2.3)
  - `isotone` (1.1-0)
  - `lubridate` (1.8.0)
  - `patchwork` (1.1.1)
  - `quantreg` (5.88)
  - `tidyverse` (1.3.1)
  - and for preprocessing data: `MMWRweek` (0.1.3), `jsonlite` (1.8.0)

## References

Cramer EY, Huang Y, Wang Y, et al. 2021. The United States COVID-19 Forecast Hub
dataset. _medRxiv_. URL: https://www.medrxiv.org/content/10.1101/2021.11.04.21265886v1

Koenker R. 2022. quantreg: Quantile Regression. R package version 5.88. URL: https://CRAN.R-project.org/package=quantreg
