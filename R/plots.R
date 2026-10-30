# ------------------------------------------------------------------------------
# Figures: forest plots, caterpillar plot, county ranking, calibration, gradients.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

theme_pub <- function(base = 11) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA), strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' Forest plot of odds ratios (log scale) grouped by variable
forest_plot <- function(tab, title = "Adjusted odds ratios", xlab = "Odds ratio (log scale, 95% CI)") {
  tab <- tab[!is.na(tab$variable), ]
  tab$label <- factor(paste(tab$variable, tab$category, sep = ": "), levels = rev(paste(tab$variable, tab$category, sep = ": ")))
  ggplot2::ggplot(tab, ggplot2::aes(or, label, colour = variable)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_low, xmax = ci_high), height = 0.25) + ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8, 16)) + ggplot2::guides(colour = "none") +
    ggplot2::labs(title = title, x = xlab, y = NULL) + theme_pub(9)
}

#' Caterpillar plot of county random effects (odds ratio versus the average county)
caterpillar_plot <- function(re_tab, title = "County effects on the odds of adequate ANC") {
  re_tab <- re_tab[order(re_tab$u), ]; re_tab$county <- factor(re_tab$county, levels = re_tab$county)
  ggplot2::ggplot(re_tab, ggplot2::aes(exp(u), county)) + ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = exp(lower), xmax = exp(upper)), height = 0, colour = "grey55") +
    ggplot2::geom_point(colour = "#1b4965", size = 1.6) + ggplot2::scale_x_log10() +
    ggplot2::labs(title = title, x = "Odds ratio versus the average county (95% interval)", y = NULL) + theme_pub(8)
}

#' Ranked county estimates with design-based confidence intervals
county_rank_plot <- function(est, overall = NULL, title = "Adequate ANC by county", ylab = "Weighted percentage (95% CI)") {
  est <- est[order(est$pct), ]; est$county <- factor(est$county, levels = est$county)
  g <- ggplot2::ggplot(est, ggplot2::aes(pct, county, colour = small_n)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = low, xmax = high), height = 0, linewidth = 0.4) + ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#1b4965", `TRUE` = "#c1121f"), labels = c("n at or above threshold", "Small n (imprecise)"), name = NULL) +
    ggplot2::labs(title = title, x = ylab, y = NULL) + theme_pub(8) + ggplot2::theme(legend.position = "bottom")
  if (!is.null(overall)) g <- g + ggplot2::geom_vline(xintercept = overall, linetype = "dotted")
  g
}

#' Calibration plot (deciles of predicted risk)
calibration_plot <- function(y, p, w = rep(1, length(y)), g = 10) {
  o <- order(p); y <- y[o]; p <- p[o]; w <- w[o]
  grp <- pmin(g, ceiling(g * cumsum(w) / sum(w)))
  d <- data.frame(pred = tapply(w * p, grp, sum) / tapply(w, grp, sum), obs = tapply(w * y, grp, sum) / tapply(w, grp, sum))
  ggplot2::ggplot(d, ggplot2::aes(pred, obs)) + ggplot2::geom_abline(linetype = "dashed", colour = "grey50") + ggplot2::geom_point(size = 2.2, colour = "#1b4965") +
    ggplot2::geom_line(colour = "#1b4965") + ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(title = "Calibration by decile of predicted risk", x = "Mean predicted probability", y = "Observed proportion") + theme_pub()
}

#' Prevalence gradient plot for one factor (point estimates with CI)
gradient_plot <- function(prev, title) {
  prev$category <- factor(prev$category, levels = prev$category)
  ggplot2::ggplot(prev, ggplot2::aes(category, estimate, group = 1)) + ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, fill = "#1b4965") +
    ggplot2::geom_line(colour = "#1b4965") + ggplot2::geom_point(colour = "#1b4965", size = 2) +
    ggplot2::labs(title = title, x = NULL, y = "Weighted percentage (95% CI)") + theme_pub() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
}

#' Concentration curve for an outcome ranked by a socioeconomic factor
concentration_curve <- function(df, y = "adequate_anc", rank_var = "wealth") {
  d <- df[df$in_sample, ]; o <- order(as.numeric(d[[rank_var]])); d <- d[o, ]
  w <- d$wt / sum(d$wt); L <- cumsum(w * d[[y]]) / sum(w * d[[y]]); P <- cumsum(w)
  ggplot2::ggplot(data.frame(P = c(0, P), L = c(0, L)), ggplot2::aes(P, L)) + ggplot2::geom_abline(colour = "grey50", linetype = "dashed") +
    ggplot2::geom_line(colour = "#1b4965", linewidth = 1) + ggplot2::coord_equal() +
    ggplot2::labs(title = "Concentration curve", subtitle = paste("Outcome ranked by", rank_var), x = "Cumulative share of women, poorest first", y = "Cumulative share of the outcome") + theme_pub()
}
