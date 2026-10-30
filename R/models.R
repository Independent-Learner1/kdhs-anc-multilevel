# ------------------------------------------------------------------------------
# Regression models: survey-weighted logistic, two-level logistic, fit statistics.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

model_formula <- function(cfg, outcome = "adequate_anc", extra = NULL) {
  stats::as.formula(paste(outcome, "~", paste(c(cfg$analysis$covariates, extra), collapse = " + ")))
}

#' Odds ratios with design-based confidence intervals (t distribution on design df)
tidy_or <- function(fit, des = NULL, level = 0.95) {
  b <- stats::coef(fit); se <- sqrt(diag(stats::vcov(fit)))
  crit <- if (!is.null(des)) stats::qt(1 - (1 - level) / 2, survey::degf(des)) else stats::qnorm(1 - (1 - level) / 2)
  z <- b / se
  p <- if (!is.null(des)) 2 * stats::pt(-abs(z), survey::degf(des)) else 2 * stats::pnorm(-abs(z))
  tibble::tibble(term = names(b), or = exp(b), ci_low = exp(b - crit * se), ci_high = exp(b + crit * se), p_value = p)
}

#' Survey-weighted logistic regression (Model 1)
fit_survey_logit <- function(df, formula, subset_col = "in_sample") {
  d <- df[df[[subset_col]], ]
  d <- stats::model.frame(formula, d, na.action = stats::na.omit) |>
    cbind(d[rownames(stats::model.frame(formula, d, na.action = stats::na.omit)), c("psu", "strata", "wt", "county")])
  des <- make_design(d)
  fit <- survey::svyglm(formula, des, family = stats::quasibinomial())
  list(fit = fit, design = des, data = d)
}

#' Nagelkerke R2 with frequency weights (the same definition as SPSS LOGISTIC REGRESSION).
#' The log likelihood is computed directly because glm() rounds non-integer weights.
nagelkerke_r2 <- function(formula, data) {
  mf <- stats::model.frame(formula, data = data, weights = wt, na.action = stats::na.omit)
  y <- stats::model.response(mf); w <- stats::model.weights(mf)
  fit <- suppressWarnings(stats::glm(formula, data = data, weights = wt, family = stats::quasibinomial()))
  p <- fit$fitted.values
  ll1 <- sum(w * (y * log(p) + (1 - y) * log(1 - p)))
  p0 <- sum(w * y) / sum(w); ll0 <- sum(w * (y * log(p0) + (1 - y) * log(1 - p0)))
  n <- sum(w)
  cox <- 1 - exp((2 / n) * (ll0 - ll1))
  cox / (1 - exp((2 / n) * ll0))
}

#' Weighted Hosmer-Lemeshow test: ten groups of equal total weight, ordered by predicted risk
hosmer_lemeshow <- function(y, p, w = rep(1, length(y)), g = 10) {
  o <- order(p); y <- y[o]; p <- p[o]; w <- w[o]
  grp <- pmin(g, ceiling(g * cumsum(w) / sum(w)))
  obs <- tapply(w * y, grp, sum); exp_ <- tapply(w * p, grp, sum); n <- tapply(w, grp, sum)
  chi <- sum((obs - exp_)^2 / (exp_ * (1 - exp_ / n)))
  df_ <- length(unique(grp)) - 2
  list(chisq = chi, df = df_, p_value = stats::pchisq(chi, df_, lower.tail = FALSE))
}

#' Area under the ROC curve (unweighted; discrimination of the fitted model)
model_auc <- function(y, p) {
  r <- pROC::roc(y, p, quiet = TRUE)
  list(auc = as.numeric(pROC::auc(r)), ci = as.numeric(pROC::ci.auc(r)), roc = r)
}

#' Generalised variance inflation factors from an unweighted linear surrogate
vif_table <- function(formula, data) {
  X <- stats::model.matrix(formula, data)[, -1, drop = FALSE]
  terms_ <- attr(stats::terms(formula), "term.labels")
  assign <- attr(stats::model.matrix(formula, data), "assign")[-1]
  R <- stats::cor(X); detR <- det(R)
  out <- lapply(seq_along(terms_), function(i) {
    idx <- which(assign == i)
    gvif <- det(R[idx, idx, drop = FALSE]) * det(R[-idx, -idx, drop = FALSE]) / detR
    data.frame(term = terms_[i], df = length(idx), gvif = gvif, gvif_adjusted = gvif^(1 / (2 * length(idx))))
  })
  dplyr::bind_rows(out)
}

#' Standardised (average marginal) predicted probabilities for each level of a factor
standardised_predictions <- function(fit, data, var) {
  lv <- levels(data[[var]])
  dplyr::bind_rows(lapply(lv, function(l) {
    d <- data; d[[var]] <- factor(l, levels = lv)
    p <- stats::predict(fit, newdata = d, type = "response")
    data.frame(variable = var_labels[[var]], category = l, predicted_pct = 100 * stats::weighted.mean(as.numeric(p), data$wt))
  }))
}

# ---- Two-level (multilevel) logistic regression --------------------------------

#' Fit a random-intercept logistic model with women nested in counties
fit_glmm <- function(formula, data, cfg, group = "county") {
  f <- stats::update(formula, stats::as.formula(paste(". ~ . + (1 |", group, ")")))
  lme4::glmer(f, data = data, family = stats::binomial(), nAGQ = cfg$multilevel$nAGQ,
              control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
}

#' County variance, ICC (latent response), MOR from a glmer fit
variance_measures <- function(fit) {
  tau2 <- as.numeric(lme4::VarCorr(fit)[[1]][1, 1])
  data.frame(tau2 = tau2, icc_pct = 100 * tau2 / (tau2 + pi^2 / 3), mor = exp(sqrt(2 * tau2) * stats::qnorm(0.75)))
}

#' Proportional change in variance between null and adjusted models
pcv <- function(null_fit, full_fit) {
  t0 <- variance_measures(null_fit)$tau2; t1 <- variance_measures(full_fit)$tau2
  100 * (t0 - t1) / t0
}

#' Likelihood ratio test of the county random intercept (null glmer against null glm).
#' The p value is halved because a variance is tested on the boundary of its parameter space.
lrt_random_intercept <- function(null_glmer, data) {
  glm0 <- stats::glm(stats::formula(null_glmer, fixed.only = TRUE), data = data, family = stats::binomial())
  chi <- 2 * (as.numeric(stats::logLik(null_glmer)) - as.numeric(stats::logLik(glm0)))
  data.frame(chisq = chi, p_value = 0.5 * stats::pchisq(chi, 1, lower.tail = FALSE))
}

#' Fixed-effect odds ratios from a glmer fit (Wald intervals)
glmm_or <- function(fit, level = 0.95) {
  s <- summary(fit)$coefficients; crit <- stats::qnorm(1 - (1 - level) / 2)
  tibble::tibble(term = rownames(s), or = exp(s[, 1]), ci_low = exp(s[, 1] - crit * s[, 2]),
                 ci_high = exp(s[, 1] + crit * s[, 2]), p_value = s[, 4])
}

#' County random effects (BLUPs) with conditional standard deviations
county_random_effects <- function(fit) {
  re <- lme4::ranef(fit, condVar = TRUE)[[1]]
  pv <- attr(re, "postVar")
  tibble::tibble(county = rownames(re), u = re[, 1], se = sqrt(as.numeric(pv)),
                 lower = u - 1.96 * se, upper = u + 1.96 * se,
                 odds_ratio_vs_average = exp(u))
}

#' Dissect model terms into a tidy label for tables and plots
label_terms <- function(tab, vars) {
  tab$variable <- NA_character_; tab$category <- NA_character_
  for (v in vars) {
    hit <- startsWith(tab$term, v)
    tab$variable[hit] <- var_labels[[v]]; tab$category[hit] <- sub(paste0("^", v), "", tab$term[hit])
  }
  tab
}

# ---- Inequality measures ---------------------------------------------------------

#' Weighted concentration index of an outcome ranked by a socioeconomic variable
#' (Kakwani/Wagstaff form: CI = 2 cov_w(y, rank) / mean_w(y)). Erreygers correction for a binary outcome.
concentration_index <- function(y, rank_var, w) {
  o <- order(rank_var); y <- y[o]; w <- w[o]; w_n <- w / sum(w)
  frac_rank <- cumsum(w_n) - 0.5 * w_n
  mu <- sum(w_n * y)
  ci <- 2 * sum(w_n * (y - mu) * (frac_rank - sum(w_n * frac_rank))) / mu
  c(ci = ci, erreygers = 4 * mu * ci)
}

#' Slope (SII) and relative (RII) index of inequality using the ridit score of an ordered factor
inequality_indices <- function(des, outcome, ordered_var) {
  d <- des$variables
  lv <- levels(d[[ordered_var]]); w <- tapply(d$wt, d[[ordered_var]], sum)
  ridit <- (cumsum(w) - 0.5 * w) / sum(w); d$ridit <- unname(ridit[as.character(d[[ordered_var]])])
  des2 <- stats::update(des, ridit = d$ridit)
  fit <- survey::svyglm(stats::as.formula(paste(outcome, "~ ridit")), des2, family = stats::gaussian())
  b <- stats::coef(fit)[["ridit"]]; se <- sqrt(stats::vcov(fit)["ridit", "ridit"]); mu <- survey::svymean(stats::as.formula(paste("~", outcome)), des)[1]
  data.frame(sii_pts = 100 * b, sii_low = 100 * (b - 1.96 * se), sii_high = 100 * (b + 1.96 * se), rii = (b / mu), mean_pct = 100 * mu)
}

#' Cluster (PSU) bootstrap confidence interval for the concentration index
bootstrap_concentration <- function(df, y = "adequate_anc", rank_var = "wealth", B = 500, seed = 2022) {
  set.seed(seed); d <- df[df$in_sample, ]
  rk <- as.numeric(d[[rank_var]])
  est <- concentration_index(d[[y]], rk, d$wt)
  key <- paste(d$strata, d$psu); split_idx <- split(seq_len(nrow(d)), key)
  strata_of <- vapply(split_idx, function(i) d$strata[i[1]], numeric(1))
  boots <- replicate(B, {
    idx <- unlist(lapply(split(names(split_idx), strata_of), function(k) split_idx[sample(k, length(k), replace = TRUE)]), use.names = FALSE)
    concentration_index(d[[y]][idx], rk[idx], d$wt[idx])
  })
  data.frame(measure = c("Concentration index", "Erreygers corrected index"), estimate = est,
             ci_low = apply(boots, 1, stats::quantile, 0.025), ci_high = apply(boots, 1, stats::quantile, 0.975), row.names = NULL)
}
