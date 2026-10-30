# ------------------------------------------------------------------------------
# Utilities: configuration, package checks, small helpers.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

#' Locate the repository root (the folder that contains config/config.yml)
repo_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = TRUE)
  for (i in 1:6) {
    if (file.exists(file.path(d, "config", "config.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Could not find config/config.yml. Open the project from the repository folder.")
}

#' Read the YAML configuration. The path can be overridden with KDHS_CONFIG.
load_config <- function(path = Sys.getenv("KDHS_CONFIG", unset = NA)) {
  root <- repo_root()
  if (is.na(path) || path == "") path <- file.path(root, "config", "config.yml")
  if (!file.exists(path)) path <- file.path(root, path)
  cfg <- yaml::read_yaml(path)
  cfg$root <- root
  resolve <- function(p) if (is.null(p) || is.na(p) || p == "") NULL else if (grepl("^(/|[A-Za-z]:)", p)) p else file.path(root, p)
  # Environment variables override the config (handy for tests and for batch runs)
  data_file <- Sys.getenv("KDHS_DATA", unset = cfg$paths$data_file)
  bnd_file  <- Sys.getenv("KDHS_BOUNDARIES", unset = if (is.null(cfg$paths$boundaries_file)) "" else cfg$paths$boundaries_file)
  out_dir   <- Sys.getenv("KDHS_OUTPUT", unset = cfg$paths$output_dir)
  cfg$paths$data <- resolve(data_file)
  cfg$paths$boundaries <- resolve(bnd_file)
  cfg$paths$out <- resolve(out_dir)
  for (sub in c("data", "tables", "figures", "maps", "models")) {
    dir.create(file.path(cfg$paths$out, sub), showWarnings = FALSE, recursive = TRUE)
  }
  cfg
}

#' Check that the required packages are installed and load the core ones.
require_packages <- function(pkgs, optional = character()) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("Missing R packages: ", paste(miss, collapse = ", "),
         "\nRun source('scripts/install_packages.R') first.", call. = FALSE)
  }
  miss_opt <- optional[!vapply(optional, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss_opt)) {
    message("Optional packages not installed (some outputs will be skipped): ",
            paste(miss_opt, collapse = ", "))
  }
  invisible(TRUE)
}

#' Normalise a name for matching: lower case, no spaces, hyphens, apostrophes,
#' punctuation or the word "county".
norm_name <- function(x) {
  x <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
  x <- gsub("county", "", x)
  gsub("[^a-z0-9]", "", x)
}

#' Format a p value for tables
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f")))

#' Save a table to outputs/tables as CSV and return it unchanged
save_table <- function(df, name, cfg) {
  utils::write.csv(df, file.path(cfg$paths$out, "tables", paste0(name, ".csv")), row.names = FALSE)
  invisible(df)
}

#' Save a ggplot to outputs/figures as PNG and PDF
save_fig <- function(p, name, cfg, width = 8, height = 6, dpi = 300) {
  ggplot2::ggsave(file.path(cfg$paths$out, "figures", paste0(name, ".png")), p, width = width, height = height, dpi = dpi, bg = "white")
  ggplot2::ggsave(file.path(cfg$paths$out, "figures", paste0(name, ".pdf")), p, width = width, height = height)
  invisible(p)
}
