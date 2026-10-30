# ------------------------------------------------------------------------------
# Data preparation: read the DHS IR file and build the analysis variables.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

#' Convert DHS/SPSS variable names to clean lower-case R names (M14$1 -> m14_1)
clean_dhs_names <- function(x) gsub("[^a-z0-9]+", "_", tolower(x)) |> sub(pattern = "_$", replacement = "")

#' Read only the needed variables from a DHS .SAV or .DTA file (the IR file is large)
read_dhs_ir <- function(path, vars) {
  stopifnot(file.exists(path))
  ext <- tolower(tools::file_ext(path))
  reader <- switch(ext, sav = haven::read_sav, dta = haven::read_dta,
                   stop("Use a .SAV or .DTA file, or convert your file first."))
  header <- reader(path, n_max = 0)
  keep <- names(header)[clean_dhs_names(names(header)) %in% vars]
  missing_vars <- setdiff(vars, clean_dhs_names(keep))
  if (length(missing_vars)) {
    warning("Variables not found in the file and set to NA: ", paste(missing_vars, collapse = ", "))
  }
  raw <- reader(path, col_select = dplyr::all_of(keep))
  names(raw) <- clean_dhs_names(names(raw))
  raw
}

#' All DHS variables requested in the configuration
config_vars <- function(cfg) unique(unlist(cfg$variables))

#' Build the analysis dataset (one row per woman) from the raw IR extract.
#' Returns a tibble with factors, outcomes, sample flags and the sampling weight.
build_analysis_data <- function(raw, cfg) {
  v <- cfg$variables; a <- cfg$analysis
  col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA_real_, nrow(raw))
  num <- function(nm) as.numeric(haven::zap_labels(col(nm)))

  # County labels come from the DHS value labels of the geography variable.
  geo <- col(v$geography)
  county <- if (haven::is.labelled(geo)) as.character(haven::as_factor(geo)) else as.character(geo)
  county_code <- as.integer(haven::zap_labels(geo))

  visits <- num(v$anc_visits); visits[visits >= 98] <- NA
  first_m <- num(v$anc_first_month); first_m[first_m >= 98] <- NA
  since <- num(v$months_since_birth)

  eligible <- !is.na(since) & since <= a$recent_birth_months_max

  age <- num(v$age); par <- num(v$parity); mar <- num(v$marital); dec <- num(v$decision)
  dist <- num(v$distance); net <- num(v$internet)
  media_raw <- sapply(v$media, num)
  weekly <- apply(media_raw, 1, function(r) if (all(is.na(r))) NA_real_ else as.numeric(any(r >= 2, na.rm = TRUE)))

  df <- tibble::tibble(
    county_code = county_code, county = county,
    psu = num(v$psu), strata = num(v$strata), wt = num(v$weight) / 1e6,
    anc_visits = visits, first_month = first_m, eligible = eligible,
    adequate_anc = as.integer(ifelse(is.na(visits), NA, visits >= a$adequate_threshold)),
    optimal_anc  = as.integer(ifelse(is.na(visits), NA, visits >= a$optimal_threshold)),
    age_group = cut(age, c(14, 19, 24, 29, 34, 39, 49),
                    labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-49")),
    education = factor(num(v$education), 0:3, c("No education", "Primary", "Secondary", "Higher")),
    wealth = factor(num(v$wealth), 1:5, c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    residence = factor(num(v$residence), c(2, 1), c("Rural", "Urban")),
    parity = factor(dplyr::case_when(par <= 1 ~ 1, par <= 3 ~ 2, par >= 4 ~ 3), 1:3,
                    c("One child", "Two to three", "Four or more")),
    decision = factor(dplyr::case_when(dec %in% 1:2 ~ 1, dec %in% 3:6 ~ 2,
                                       is.na(dec) & !(mar %in% 1) ~ 3), 1:3,
                      c("Respondent involved", "Decided by others", "Not in a union (not asked)")),
    distance = factor(dplyr::case_when(dist == 2 ~ 0, dist == 1 ~ 1, TRUE ~ 2), 0:2,
                      c("Not a big problem", "Big problem", "Unknown (not asked)")),
    media_weekly = factor(weekly, 0:1, c("No weekly exposure", "Weekly exposure")),
    internet_use = factor(dplyr::case_when(net == 0 ~ 0, net %in% 1:3 ~ 1), 0:1, c("Never used", "Ever used")),
    timing = factor(dplyr::case_when(
      visits == 0 ~ 0, visits > 0 & first_m >= 0 & first_m <= a$early_booking_month ~ 1,
      visits > 0 & first_m > a$early_booking_month ~ 2, visits > 0 & is.na(first_m) ~ 3), 0:3,
      c("No ANC visit", "At 3 months or earlier", "After 3 months", "Timing not recorded")),
    early_booking = factor(dplyr::case_when(visits > 0 & first_m >= 0 & first_m <= a$early_booking_month ~ 1,
                                            visits > 0 & first_m > a$early_booking_month ~ 0),
                           0:1, c("After 3 months", "At 3 months or earlier"))
  )
  df$in_sample <- df$eligible & !is.na(df$adequate_anc)
  df$in_timing_sample <- df$in_sample & !is.na(df$early_booking)
  df$complete_case <- df$in_timing_sample & df$distance != "Unknown (not asked)" &
    df$decision != "Not in a union (not asked)" & !is.na(df$media_weekly) & !is.na(df$internet_use)
  df
}

#' Make the survey design object (strata, clusters, weights) for a subset
make_design <- function(df) {
  options(survey.lonely.psu = "adjust")
  survey::svydesign(ids = ~psu, strata = ~strata, weights = ~wt, data = df, nest = TRUE)
}

#' Sample flow: numbers at each step (for STROBE item 13)
sample_flow <- function(df) {
  tibble::tibble(
    step = c("Women in the IR file", "With a birth in the look-back window", "With a recorded number of ANC visits (analysis sample)",
             "With a recorded first-visit month (timing sample)", "Complete cases on all covariates"),
    n = c(nrow(df), sum(df$eligible), sum(df$in_sample), sum(df$in_timing_sample), sum(df$complete_case)),
    weighted_n = c(sum(df$wt), sum(df$wt[df$eligible]), sum(df$wt[df$in_sample]),
                   sum(df$wt[df$in_timing_sample]), sum(df$wt[df$complete_case]))
  )
}

#' Missing-data and structural-missingness summary for the covariates
missingness_table <- function(df, cfg) {
  d <- df[df$in_sample, ]
  covs <- cfg$analysis$covariates
  tibble::tibble(
    variable = covs,
    n_missing = vapply(covs, function(x) sum(is.na(d[[x]])), numeric(1)),
    pct_missing = round(100 * n_missing / nrow(d), 2),
    n_structural_category = vapply(covs, function(x) sum(grepl("not asked", as.character(d[[x]]))), numeric(1)),
    pct_structural_category = round(100 * n_structural_category / nrow(d), 2)
  )
}
