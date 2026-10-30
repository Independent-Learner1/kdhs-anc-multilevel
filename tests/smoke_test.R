# Smoke test: runs the full pipeline on synthetic data and checks the key outputs exist.
#   Rscript tests/smoke_test.R
tmp <- file.path(tempdir(), "kdhs_smoke"); dir.create(tmp, recursive = TRUE)
system2("Rscript", c("tests/make_synthetic.R", shQuote(tmp)))
Sys.setenv(KDHS_DATA = file.path(tmp, "synthetic_IR.sav"), KDHS_BOUNDARIES = file.path(tmp, "synthetic_counties.geojson"),
           KDHS_OUTPUT = file.path(tmp, "out"))
source("scripts/run_pipeline.R")
need <- c("tables/04_variance_measures.csv", "tables/05_global_moran.csv", "figures/map_01_observed.png", "reports/06_maps.html")
ok <- file.exists(file.path(tmp, "out", need)); names(ok) <- need; print(ok)
if (!all(ok)) stop("Smoke test failed") else message("Smoke test passed.")
