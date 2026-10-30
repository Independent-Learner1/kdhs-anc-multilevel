# ------------------------------------------------------------------------------
# Geospatial epidemiology: county estimates, neighbours, spatial autocorrelation,
# hot spots, empirical Bayes smoothing and an exploratory spatial regression.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

#' Read county boundaries and attach a matching key.
#'
#' The name field is taken from `spatial.boundary_name_field`; if it is absent,
#' common alternatives (shapeName, NAME_1, COUNTY_NAM, ADM1_EN, ...) are tried.
read_boundaries <- function(cfg) {
  path <- cfg$paths$boundaries
  if (is.null(path) || !file.exists(path)) {
    stop("Boundary file not found: ", path, "\nSee data/boundaries/README.md for sources and the required attribute.", call. = FALSE)
  }
  bnd <- sf::st_make_valid(sf::st_read(path, quiet = TRUE))
  bnd <- sf::st_transform(bnd, 4326)
  wanted <- cfg$spatial$boundary_name_field
  cand <- c(wanted, "county", "COUNTY", "County", "shapeName", "NAME_1", "ADM1_EN", "COUNTY_NAM", "name", "NAME")
  hit <- cand[cand %in% names(bnd)][1]
  if (is.na(hit)) stop("No county name attribute found. Columns: ", paste(names(bnd), collapse = ", "), call. = FALSE)
  bnd$county_boundary <- as.character(bnd[[hit]])
  bnd$county_key <- norm_name(bnd$county_boundary)
  al <- cfg$spatial$name_aliases
  if (length(al)) {
    for (k in names(al)) bnd$county_key[bnd$county_key == k] <- al[[k]]
  }
  bnd
}

#' Join survey county names to boundary polygons. Stops with a clear message when a
#' survey county has no polygon, so a silent mismatch cannot reach a map.
match_boundaries <- function(bnd, counties, cfg) {
  key <- norm_name(counties)
  miss <- counties[!key %in% bnd$county_key]
  if (length(miss)) {
    stop("Survey counties without a boundary: ", paste(miss, collapse = ", "),
         "\nBoundary names available: ", paste(sort(bnd$county_boundary), collapse = ", "),
         "\nAdd spellings to spatial.name_aliases in the config.", call. = FALSE)
  }
  extra <- bnd$county_boundary[!bnd$county_key %in% key]
  if (length(extra)) message("Boundary polygons without survey data (kept as NA): ", paste(extra, collapse = ", "))
  bnd
}

#' Design-based county estimates with logit confidence intervals (design degrees of freedom).
county_estimates <- function(df, cfg, outcome = "adequate_anc") {
  des <- make_design(df)
  des <- subset(des, in_sample)
  level <- cfg$analysis$conf_level
  cty <- sort(unique(as.character(df$county[df$in_sample])))
  out <- lapply(cty, function(cn) {
    sub <- subset(des, as.character(county) == cn)
    fml <- stats::as.formula(paste0("~", outcome))
    est <- tryCatch(survey::svyciprop(fml, sub, method = "logit", level = level), error = function(e) NULL)
    n <- sum(df$in_sample & as.character(df$county) == cn)
    if (is.null(est)) return(data.frame(county = cn, n = n, pct = NA, low = NA, high = NA, se = NA))
    ci <- stats::confint(est)
    data.frame(county = cn, n = n, pct = 100 * as.numeric(est), low = 100 * ci[1], high = 100 * ci[2],
               se = 100 * as.numeric(survey::SE(est)))
  })
  out <- do.call(rbind, out)
  out$small_n <- out$n < cfg$analysis$small_county_n
  out$county_key <- norm_name(out$county)
  out
}

#' Neighbours: queen contiguity (or kNN), with a kNN fallback for islands.
#' Returns the nb object and a row-standardised listw.
build_neighbours <- function(bnd, cfg) {
  sf::sf_use_s2(FALSE)
  ctr <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(bnd))))
  nb <- if (identical(cfg$spatial$weights_type, "knn")) {
    spdep::knn2nb(spdep::knearneigh(ctr, k = cfg$spatial$knn))
  } else {
    spdep::poly2nb(bnd, queen = TRUE)
  }
  iso <- which(spdep::card(nb) == 0)
  if (length(iso)) {
    message("Units without contiguous neighbours (", paste(bnd$county_boundary[iso], collapse = ", "), "): linked to nearest ", cfg$spatial$knn, " centroids.")
    knn <- spdep::knn2nb(spdep::knearneigh(ctr, k = cfg$spatial$knn))
    for (i in iso) nb[[i]] <- as.integer(knn[[i]])
    nb <- spdep::make.sym.nb(nb)
  }
  list(nb = nb, lw = spdep::nb2listw(nb, style = "W", zero.policy = TRUE))
}

#' Global Moran's I with Monte Carlo permutation inference
global_moran <- function(x, lw, nsim = 9999, seed = 2022) {
  set.seed(seed)
  mc <- spdep::moran.mc(x, lw, nsim = nsim, zero.policy = TRUE)
  tibble::tibble(statistic = unname(mc$statistic), p_value = mc$p.value, nsim = nsim,
                 expected = -1 / (length(x) - 1))
}

#' Local Moran's I (LISA) with permutation p values and quadrant classes
local_moran <- function(x, lw, nsim = 9999, alpha = 0.05, seed = 2022) {
  set.seed(seed)
  lm <- spdep::localmoran_perm(x, lw, nsim = nsim, zero.policy = TRUE)
  z <- as.numeric(scale(x)); lag_z <- spdep::lag.listw(lw, z, zero.policy = TRUE)
  pcol <- grep("^Pr\\(z", colnames(lm), value = TRUE)
  pcol <- pcol[!grepl("Sim", pcol)][1]
  p <- lm[, pcol]
  cls <- ifelse(z >= 0 & lag_z >= 0, "High-High", ifelse(z < 0 & lag_z < 0, "Low-Low",
         ifelse(z >= 0 & lag_z < 0, "High-Low", "Low-High")))
  tibble::tibble(local_i = lm[, "Ii"], p_lisa = p, p_lisa_fdr = stats::p.adjust(p, "fdr"), z = z, lag_z = lag_z,
                 cluster = ifelse(p < alpha, cls, "Not significant"),
                 cluster_fdr = ifelse(stats::p.adjust(p, "fdr") < alpha, cls, "Not significant"))
}

#' Getis-Ord Gi* hot and cold spots (self included in the neighbourhood)
getis_ord <- function(x, nb, alpha = 0.05) {
  lw_star <- spdep::nb2listw(spdep::include.self(nb), style = "B", zero.policy = TRUE)
  g <- as.numeric(spdep::localG(x, lw_star, zero.policy = TRUE))
  p <- 2 * stats::pnorm(-abs(g)); pf <- stats::p.adjust(p, "fdr")
  tibble::tibble(gi_star = g, p_gi = p, p_gi_fdr = pf,
                 hotspot = ifelse(p < alpha & g > 0, "Hot spot", ifelse(p < alpha & g < 0, "Cold spot", "Not significant")),
                 hotspot_fdr = ifelse(pf < alpha & g > 0, "Hot spot", ifelse(pf < alpha & g < 0, "Cold spot", "Not significant")))
}

#' Empirical Bayes smoothing of county proportions (global and local, Marshall)
#' Counts are taken as round(prop * n); with design effects the smoothing is approximate.
eb_smooth <- function(prop, n, nb) {
  r <- round(prop * n)
  g <- spdep::EBest(r, n)
  l <- tryCatch(spdep::EBlocal(r, n, nb, zero.policy = TRUE), error = function(e) NULL)
  tibble::tibble(eb_global = 100 * g$estmm, eb_local = if (is.null(l)) NA_real_ else 100 * l$est)
}

#' Weighted county-level context variables defined in the config
county_context <- function(df, cfg) {
  des <- subset(make_design(df), in_sample)
  spec <- cfg$spatial$contextual
  cty <- sort(unique(as.character(df$county[df$in_sample])))
  res <- data.frame(county = cty, county_key = norm_name(cty))
  for (s in spec) {
    ind <- as.numeric(des$variables[[s$var]] %in% unlist(s$levels))
    des2 <- stats::update(des, ind__ = ind)
    m <- survey::svyby(~ind__, ~county, des2, survey::svymean, na.rm = TRUE)
    res[[s$name]] <- 100 * m$ind__[match(cty, as.character(m$county))]
  }
  res
}

#' Exploratory county-level spatial regression.
#' OLS, Lagrange multiplier tests, spatial lag and spatial error models. The unit of
#' analysis is the county (ecological), so the results describe areas, not women.
spatial_regression <- function(est_ctx, lw, response = "pct", predictors) {
  fml <- stats::reformulate(predictors, response)
  ols <- stats::lm(fml, data = est_ctx)
  lmt <- spdep::lm.LMtests(ols, lw, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"), zero.policy = TRUE)
  lmt_tab <- do.call(rbind, lapply(names(lmt), function(n) data.frame(test = n, statistic = unname(lmt[[n]]$statistic), p_value = lmt[[n]]$p.value)))
  lag <- spatialreg::lagsarlm(fml, data = est_ctx, listw = lw, zero.policy = TRUE)
  err <- spatialreg::errorsarlm(fml, data = est_ctx, listw = lw, zero.policy = TRUE)
  moran_res <- spdep::moran.test(stats::residuals(ols), lw, zero.policy = TRUE)
  cmp <- data.frame(model = c("OLS", "Spatial lag", "Spatial error"),
                    AIC = c(stats::AIC(ols), stats::AIC(lag), stats::AIC(err)),
                    logLik = c(as.numeric(stats::logLik(ols)), as.numeric(stats::logLik(lag)), as.numeric(stats::logLik(err))),
                    rho_or_lambda = c(NA, unname(lag$rho), unname(err$lambda)))
  list(ols = ols, lag = lag, err = err, lm_tests = lmt_tab, compare = cmp,
       resid_moran = tibble::tibble(I = unname(moran_res$estimate[1]), p_value = moran_res$p.value))
}

#' Moran's I of the county random effects (residual geographic clustering after adjustment)
re_moran <- function(re_tab, bnd, lw, nsim = 9999, seed = 2022) {
  u <- re_tab$u[match(bnd$county_key, norm_name(re_tab$county))]
  global_moran(u, lw, nsim, seed)
}
