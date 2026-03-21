############################
# Regional warming project #
############################

**Beaulieu, Johnson, Killick, Lanzante and Knutson**  
*Under review at Nature Communications*

## Overview

This repository contains the R code required to reproduce the data analysis and 
figures for Beaulieu et al. The main goal of this project is to identify regions 
that have undergone a warming acceleration since 1970, using changepoint detection 
and quadratic trend fitting applied to gridded surface temperature datasets.

---

## Requirements

**R version:** 4.4.0

**R packages:**

| Package | Version | Purpose |
|---|---|---|
| `datasets` | 4.4.0 | Built-in R datasets (base R) |
| `ncdf4` | 1.22 | Reading and writing NetCDF climate data files |
| `terra` | 1.8-60 | Spatial raster data processing and regridding |
| `oce` | 1.8-3 | Oceanographic data analysis and visualization |
| `ggplot2` | 3.5.1 | Figure production |
| `reshape2` | 1.4.4 | Reshaping data between wide and long formats |
| `ggpubr` | 0.6.0 | Publication-ready ggplot2 figure arrangement |
| `forecast` | 8.23.0 | Time series modeling and forecasting |
| `nlme` | 3.1-164 | Linear and nonlinear mixed-effects models |
| `oceanmap` | 0.1.6 | Oceanographic mapping utilities |
| `RColorBrewer` | 1.1-3 | Color palettes for figures |
| `viridis` | 0.6.5 | Perceptually uniform color palettes for figures |
| `abind` | 1.4-8 | Combining multi-dimensional arrays |
| `ggnewscale` | 0.5.2 | Multiple color/fill scales in ggplot2 figures |

> A full record of the R session is provided in `sessionInfo.txt`. For full reproducibility, a `renv.lock` file is also included — see **Installation** below.

**Non-standard hardware:** None required. All scripts run on a standard desktop or laptop computer.

---

## Installation

1. Clone or download this repository.
2. Open R and set the working directory to the repository root.
3. To restore the exact package environment used in the paper, install `renv` and run:

```r
install.packages("renv")
renv::restore()
```
4. Alternatively, install packages manually as listed in `00_Setup.R`.

**Typical installation time:** ~10 minutes (depending on packages already installed).

---

## Data

Raw monthly gridded surface temperature files are not included in this repository due to size, but are publicly available from:
- [Berkeley,  https://berkeleyearth.org/data/]
- [NOAA, https://psl.noaa.gov/data/gridded/data.noaaglobaltemp.html]
- [NASA, https://data.giss.nasa.gov/gistemp/]
- [HadCRUT, https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/download.html]
- [DCENT-I, https://dcent-i.github.io/]

Place raw data files in the `/data/raw` folder before running the scripts. Global mean surface temperature time series should also be placed there.

---
## Usage

Scripts should be run in order from the `/code` folder:

| Step | Script | Description |
|---|---|---|
| 0 | `00_Setup.R` | Load libraries and helper functions used across all scripts |
| 1 | `01_CleanRawData.R` | Process raw monthly gridded surface temperature files; compute annual anomalies; regrid fine-resolution datasets to 5×5°; load global mean surface temperature time series |
| 2 | `02_AnalysisGriddedDatasets.R` | Analyze gridded annual surface temperature anomalies for changes in the rate of warming via changepoint detection and quadratic trend fitting |
| 3 | `03_AnalysisRegionalTS.R` | Create and analyze regional surface temperature time series (Figure 4) |
| 4 | `04_AnalysisGlobalTS.R` | Analyze global surface temperature time series (Figure S1) — *not required to reproduce main paper results* |
| 5 | `05_SimsBerkeley.R` | Conduct simulation study (Supplementary) evaluating true and false positive rates |
| 6 | `06_PlotsTempResults.R` | Produce figures from observed temperature analysis |
| 7 | `07_PlotsSims.R` | Produce figures from simulation results |

---

## Repository Structure

```
/code           # All R scripts (00–07)
/data           # Raw input data and processed data (not included; see Data section above)
/results         # Generated results files
/figures        # Generated figures
sessionInfo.txt # Full R session info for reproducibility
```

---

## License

This code is released under the [MIT / GPL-3.0] License — see `LICENSE` for details.

---

## Citation

If you use this code, please cite:

> Beaulieu, Johnson, Killick, Lanzante and Knutson.
Space-time signatures of surface warming accelerations since 1970, 
28 October 2025, PREPRINT (Version 1) available at Research Square 
[https://doi.org/10.21203/rs.3.rs-7731926/v1]

> Beaulieu, Johnson, Killick, Lanzante and Knutson. (2026). Code - Space-time 
signatures of surface warming accelerations since 1970 (v1.1.0). Zenodo. 
https://doi.org/10.5281/zenodo.19140812

---

## Contact

For questions, comments, or suggestions, please contact **Claudie Beaulieu** at beaulieu@ucsc.edu.
