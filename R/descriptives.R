# ------------------------------------------------------------------------------
# Descriptive and bivariate analysis with the complex survey design.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

#' Variable labels used in tables and figures
var_labels <- c(age_group = "Maternal age (years)", education = "Education", wealth = "Wealth quintile",
                residence = "Residence", parity = "Parity", decision = "Healthcare decision-making",
                distance = "Distance to a health facility", media_weekly = "Weekly mass media exposure",
                internet_use = "Ever used the internet", timing = "Timing of first ANC visit",
                early_booking = "First visit at 3 months or earlier")

#' Unweighted n and weighted % for each category of each variable (Table 1 style)
sample_characteristics <- function(df, vars) {
  d <- df[df$in_sample, ]
  out <- lapply(vars, function(x) {
    tab <- d |> dplyr::filter(!is.na(.data[[x]])) |>
      dplyr::group_by(category = .data[[x]]) |>
      dplyr::summarise(n = dplyr::n(), weighted_n = sum(wt), .groups = "drop") |>
      dplyr::mutate(weighted_pct = 100 * weighted_n / sum(weighted_n), variable = var_labels[[x]],
                    category = as.character(category))
    tab[, c("variable", "category", "n", "weighted_n", "weighted_pct")]
  })
  dplyr::bind_rows(out)
}

#' Design-based percentage (with logit confidence interval) of an outcome by category
prevalence_by <- function(des, outcome, by, level = 0.95) {
  f_by <- stats::as.formula(paste("~", by)); f_out <- stats::as.formula(paste("~", outcome))
  r <- survey::svyby(f_out, f_by, des, survey::svyciprop, vartype = "ci", method = "logit",
                     na.rm = TRUE, level = level, keep.names = FALSE)
  names(r) <- c("category", "estimate", "ci_low", "ci_high")
  r$category <- as.character(r$category)
  r[, c("estimate", "ci_low", "ci_high")] <- 100 * r[, c("estimate", "ci_low", "ci_high")]
  r
}

#' Cramer's V from the weighted cross-tabulation (as reported by SPSS CROSSTABS with weights on)
cramers_v <- function(df, x, y = "adequate_anc") {
  d <- df[df$in_sample & !is.na(df[[x]]) & !is.na(df[[y]]), ]
  tab <- stats::xtabs(wt ~ d[[x]] + d[[y]], data = d)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
}

#' Crude odds ratios from single-variable survey-weighted logistic regression
crude_or <- function(des, x, outcome = "adequate_anc", level = 0.95) {
  fit <- survey::svyglm(stats::as.formula(paste(outcome, "~", x)), des, family = stats::quasibinomial())
  tidy_or(fit, des, level) |> dplyr::filter(term != "(Intercept)")
}

#' Bivariate table: weighted % with 95% CI, difference from reference, crude OR, Cramer's V
bivariate_table <- function(df, cfg, vars = cfg$analysis$covariates, outcome = "adequate_anc") {
  d <- df[df$in_sample, ]
  des <- make_design(d)
  res <- lapply(vars, function(x) {
    p <- prevalence_by(des, outcome, x, cfg$analysis$conf_level)
    n <- d |> dplyr::count(category = as.character(.data[[x]]), name = "n")
    wn <- d |> dplyr::group_by(category = as.character(.data[[x]])) |> dplyr::summarise(weighted_n = sum(wt), .groups = "drop")
    o <- crude_or(des, x, outcome, cfg$analysis$conf_level)
    o$category <- sub(paste0("^", x), "", o$term)
    out <- p |> dplyr::left_join(n, by = "category") |> dplyr::left_join(wn, by = "category") |>
      dplyr::left_join(o[, c("category", "or", "ci_low", "ci_high")], by = "category", suffix = c("", "_or"))
    ref <- levels(d[[x]])[1]
    out$difference_pts <- out$estimate - out$estimate[out$category == ref]
    out$cramers_v <- cramers_v(df, x, outcome)
    out$variable <- var_labels[[x]]
    out[match(levels(d[[x]]), out$category), ]
  })
  out <- dplyr::bind_rows(res)
  names(out)[names(out) == "ci_low_or"] <- "or_ci_low"; names(out)[names(out) == "ci_high_or"] <- "or_ci_high"
  names(out)[names(out) == "ci_low"] <- "pct_ci_low"; names(out)[names(out) == "ci_high"] <- "pct_ci_high"
  out[, c("variable", "category", "n", "weighted_n", "estimate", "pct_ci_low", "pct_ci_high",
          "difference_pts", "or", "or_ci_low", "or_ci_high", "cramers_v")]
}

#' Outcome prevalence overall (design-based)
overall_prevalence <- function(df, cfg) {
  d <- df[df$in_sample, ]; des <- make_design(d)
  one <- function(outcome) {
    ci <- survey::svyciprop(stats::as.formula(paste("~", outcome)), des, method = "logit", level = cfg$analysis$conf_level)
    data.frame(outcome = outcome, estimate = 100 * as.numeric(ci), ci_low = 100 * attr(ci, "ci")[1], ci_high = 100 * attr(ci, "ci")[2])
  }
  r <- rbind(one("adequate_anc"), one("optimal_anc"))
  r$mean_visits <- survey::svymean(~anc_visits, des)[1]
  r$sd_visits <- sqrt(survey::svyvar(~anc_visits, des)[1])
  r
}
