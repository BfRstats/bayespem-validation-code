---
output: 
    github_document:
        toc: true
title: R code for the article "Validation of a generic Bayesian method for prevalence estimation under misclassification"
author: Matthias Flor
bibliography: references.bib
---

<!-- This markdown document has been gerenated by knitting an R markdown file.-->

This R project includes all relevant data and code for the validation.
The project has been organized with the help of the ProjectTemplate package and adheres to its directory layout ^[For more details about ProjectTemplate, see http://projecttemplate.net].

All of the analyses have been run already (and some took a long time to complete!), so instead of re-running them you may prefer to just look at the script files in the `src` directory and the generated plots in the `graphs` directory.
The validation study's configuration can be inspected in the `libs/globals.R` file. 
For more details on the contained directories and files, see [Project structure] below.

# Prerequisites

To load the project, follow these steps in order:

1. First clone the repository, for example by typing

    
    ```sh
    git clone https://github.com/BfRstats/bayespem-validation-code.git
    ```

    in a terminal.

1. Make sure you have the software JAGS ^[http://mcmc-jags.sourceforge.net/] installed on your system.

1. Start R (if you are using the RStudio IDE, you can just click the `bayespem-validation-code.Rproj` file). All subsequent steps are to be executed within the R environment.

1. A number of R packages is required. You may install them by calling:  

    
    ```r
    install.packages(
        c("ProjectTemplate",
          "data.table", "dplyr", "dtplyr", "lubridate", "magrittr", "stringr", "tidyr",
          "ggplot2", "hexbin", "cowplot",
          "Hmisc", "snow", "snowfall",
          "rjags", "runjags", "coda",
          "mcr", "broman",
          "knitr", "kableExtra", "latex2exp")
    )
    ```

## Software versions used for the study

**R session info:**

```
## R version 3.6.2 (2019-12-12)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.3 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] latex2exp_0.4.0       kableExtra_1.1.0      knitr_1.26            broman_0.69-5         mcr_1.2.1             runjags_2.0.4-6      
##  [7] rjags_4-10            coda_0.19-3           snowfall_1.84-6.1     snow_0.4-3            cowplot_1.0.0         hexbin_1.28.0        
## [13] ggplot2_3.2.1         tidyr_1.0.0           stringr_1.4.0         magrittr_1.5          lubridate_1.7.4       dtplyr_1.0.0         
## [19] dplyr_0.8.3           data.table_1.12.8     ProjectTemplate_0.9.0
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_0.2.5  xfun_0.11         purrr_0.3.3       lattice_0.20-38   colorspace_1.4-1  vctrs_0.2.1       htmltools_0.4.0  
##  [8] viridisLite_0.3.0 rlang_0.4.2       pillar_1.4.3      glue_1.3.1        withr_2.1.2       lifecycle_0.1.0   munsell_0.5.0    
## [15] gtable_0.3.0      rvest_0.3.5       evaluate_0.14     labeling_0.3      Rcpp_1.0.3        readr_1.3.1       scales_1.1.0     
## [22] backports_1.1.5   webshot_0.5.2     farver_2.0.1      hms_0.5.2         digest_0.6.23     stringi_1.4.3     grid_3.6.2       
## [29] tools_3.6.2       lazyeval_0.2.2    tibble_2.1.3      crayon_1.3.4      pkgconfig_2.0.3   zeallot_0.1.0     ellipsis_0.3.0   
## [36] xml2_1.2.2        assertthat_0.2.1  rmarkdown_2.0     httr_1.4.1        rstudioapi_0.10   R6_2.4.1          compiler_3.6.2
```

**JAGS:**

```
## You are using R version 3.6.2 (2019-12-12) on a unix machine, with the RStudio GUI
## JAGS version 4.3.0 found successfully using the command '/usr/bin/jags'
## The rjags package is installed
```

# Project loading

1.  Set the working directory to the directory where this README file is located (this should already be set correctly if you used the `bayespem-validation-code.Rproj` file to open the project):

    
    ```r
    setwd("PATH_TO_README_DIRECTORY")
    ```

1. Load the ProjectTemplate package:

    
    ```r
    library(ProjectTemplate)
    ```

1. Then load the project by typing 

    
    ```r
    suppressPackageStartupMessages(load.project())
    ```

    You'll see a series of automated messages while ProjectTemplate does the following:
    *  Load the project configuration.
    *  Load all required R packages.
    *  Source all helper scripts in the `lib` directory.
    *  Read in all data files stored in the `cache` directory.
    *  Recreate the simulated parameter and data sets.
    
Once that's done, the project has been fully loaded and is in a state where all variables are available in the global environment for inspection.

# Project structure

A quick run down of the directories and files contained in the project.

## cache
Contains cached data files in `.RData` format and `.hash` files that ProjectTemplate uses to check if a data file is up to date while caching..

## config
* `global.dcf`: ProjectTemplates's configuration settings.

## data
* `01-generate-datasets.R`: Code to generate the simulated parameter and data sets.

## graphs
PDF and PNG versions of the figures generated by scripts in the `src` directory.

## lib
* `globals.R`: Configuration of the validation study (number of parameter sets, etc.)
* `01-helpers.R`: Various helper functions.
* `02-ci4prev-Reiczigel_et_al.R`: Code to calculate Clopper-Pearson, Blaker, Sterne, and Lang-Reiczigel confidence limits (code adapted from @Reiczigel2010 and @Lang2014).
* `03-parallel-ci.R`: Code to calculate confidence intervals in a parallel setup (i.e., for multiple CPU's).

## src
Code for the different analyses undertaken in the study.
The file's names should be quite self-explanatory.

* `01-calculate-Rogan-Gladen-estimates.R` 
* `02-calculate-conventional-CIs.R` 
* `03-run-Bayesian-prevalence-estimation.R` 
* `04-calculate-average-bias-and-coverage.R`  
* `05-run-Deming-regressions.R` 
* `06-generate-Fig2.R` 
* `07-generate-Fig3.R`  
* `08-generate-Fig4.R` 
* `09-generate-LaTeX-tables.R` 
* `run-everything.R` 

# Replicating the study

The full study is configured via `libs/globals.R` to have 1,000 parameter sets and 1,000 replicates for a total of 1,000,000 data sets.
You can run each of the analyses in the `src` directory simply by typing `source("src/FILENAME")`.
If you want to really re-run everything, make sure to source the files in the order indicated by the numbered filenames ^[**Warning:** Depending on your hardware, the script `src/03-run-Bayesian-prevalence-estimation.R` will probably take at least days to finish.].
For convenience, an R script is provided in `src/run-everything.R` that includes all steps to reproduce the study.

If you want to check whether everything works before replicating the full study, you may switch to the `toy-study` git branch which is equivalent to the full study but has just 20 parameter sets and 20 replicates for a total of 400 data sets (again, configured via `libs/globals.R`).

# References
