# ------------------------------------------------------------------------------
# One-line setup for every notebook and script:
#   source("../R/_load.R")      # from notebooks/
#   source("R/_load.R")         # from the repository root
# Loads the functions, reads the configuration (cfg), sets the seed.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------
local({
  here <- normalizePath(getwd())
  d <- here
  for (i in 1:6) { if (file.exists(file.path(d, "config", "config.yml"))) break; d <- dirname(d) }
  rfiles <- c("utils.R", "data_prep.R", "descriptives.R", "models.R", "spatial.R", "maps.R", "plots.R")
  for (f in rfiles) source(file.path(d, "R", f), local = FALSE)
})
cfg <- load_config()
set.seed(cfg$project$seed)
suppressPackageStartupMessages({ library(survey); library(dplyr); library(ggplot2); library(sf) })
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE)
