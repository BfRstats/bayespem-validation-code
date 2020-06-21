#' Load the project:
library(ProjectTemplate)
suppressPackageStartupMessages(load.project())

#' Source these R scripts in order to reproduce the study.
source("src/01-calculate-Rogan-Gladen-estimates.R")
source("src/02-calculate-conventional-CIs.R")
source("src/03-run-Bayesian-prevalence-estimation.R")      # warning: depending on your hardware, this will probably take days to finish
source("src/04-calculate-average-bias-and-coverage.R")
source("src/05-run-Deming-regressions.R")
source("src/06-generate-Fig2.R")
source("src/07-generate-Fig3.R")
source("src/08-generate-Fig4.R")
source("src/09-generate-LaTeX-tables.R")

#' Supplementary material.
rmarkdown::render("src/supplementary_material/10-regression.Rmd")

#' Knit the README.Rmd file to update the sessionInfo
#' in the README.md file:
knitr::knit("README.Rmd")
