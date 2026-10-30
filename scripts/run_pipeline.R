# ------------------------------------------------------------------------------
# Run the whole pipeline: render the seven notebooks in order.
#   Rscript scripts/run_pipeline.R
#   Rscript scripts/run_pipeline.R 05 06        # only some notebooks
# Optional environment variables: KDHS_CONFIG, KDHS_DATA, KDHS_BOUNDARIES, KDHS_OUTPUT
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (file.exists("config/config.yml")) "." else "..")
nbs <- sort(list.files(file.path(root, "notebooks"), pattern = "^[0-9]{2}_.*\\.Rmd$", full.names = TRUE))
if (length(args)) nbs <- nbs[substr(basename(nbs), 1, 2) %in% args]
out_dir <- Sys.getenv("KDHS_OUTPUT", unset = file.path(root, "outputs"))
dir.create(file.path(out_dir, "reports"), showWarnings = FALSE, recursive = TRUE)
for (nb in nbs) {
  message("==== ", basename(nb))
  rmarkdown::render(nb, output_dir = file.path(out_dir, "reports"), envir = new.env(), quiet = TRUE)
}
message("Done. Reports: ", file.path(out_dir, "reports"), "   Tables: ", file.path(out_dir, "tables"),
        "   Figures: ", file.path(out_dir, "figures"), "   Maps: ", file.path(out_dir, "maps"))
