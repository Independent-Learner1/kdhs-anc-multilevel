# Install the R packages used by the pipeline (run once).
core <- c("yaml", "haven", "dplyr", "tidyr", "tibble", "survey", "lme4", "ggplot2", "patchwork", "viridis", "scales",
          "sf", "spdep", "spatialreg", "classInt", "pROC", "rmarkdown", "knitr", "htmlwidgets")
optional <- c("leaflet")   # interactive maps
need <- setdiff(c(core, optional), rownames(installed.packages()))
if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
message("Core packages ready. Optional: ", paste(optional, collapse = ", "))
