# ------------------------------------------------------------------------------
# Static (ggplot2) and interactive (leaflet) maps.
# Author: Briton M Kavulavu
# ------------------------------------------------------------------------------

theme_map <- function(base = 11) {
  ggplot2::theme_void(base_size = base) +
    ggplot2::theme(legend.position = "right", plot.title = ggplot2::element_text(face = "bold", size = base + 2),
                   plot.subtitle = ggplot2::element_text(colour = "grey30"), plot.caption = ggplot2::element_text(colour = "grey40", size = base - 3, hjust = 0),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

#' Quantile classes with readable labels
quantile_classes <- function(x, k = 5, digits = 1) {
  br <- unique(stats::quantile(x, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE))
  br[1] <- br[1] - 1e-6; br[length(br)] <- br[length(br)] + 1e-6
  cut(x, br, include.lowest = TRUE, labels = paste0(formatC(br[-length(br)], digits, format = "f"), " to ", formatC(br[-1], digits, format = "f")))
}

#' Choropleth of a continuous county quantity in quantile classes.
#' Counties flagged `small_n` get a dashed outline.
map_choropleth <- function(sf_obj, value, title, legend = "%", k = 5, subtitle = NULL, caption = NULL, palette = "viridis") {
  sf_obj$.cls <- quantile_classes(sf_obj[[value]], k)
  g <- ggplot2::ggplot(sf_obj) +
    ggplot2::geom_sf(ggplot2::aes(fill = .cls), colour = "grey20", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_d(option = palette, direction = -1, na.value = "grey85", name = legend, drop = FALSE) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) + theme_map()
  if ("small_n" %in% names(sf_obj) && any(sf_obj$small_n, na.rm = TRUE)) {
    g <- g + ggplot2::geom_sf(data = sf_obj[which(sf_obj$small_n), ], fill = NA, colour = "black", linewidth = 0.6, linetype = "dashed")
  }
  g
}

#' Diverging map (e.g. county random effects on the odds-ratio scale, centred at zero)
map_diverging <- function(sf_obj, value, title, legend, subtitle = NULL, caption = NULL) {
  lim <- max(abs(sf_obj[[value]]), na.rm = TRUE)
  ggplot2::ggplot(sf_obj) + ggplot2::geom_sf(ggplot2::aes(fill = .data[[value]]), colour = "grey20", linewidth = 0.2) +
    ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, limits = c(-lim, lim), name = legend, na.value = "grey85") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) + theme_map()
}

lisa_colours <- c("High-High" = "#b2182b", "Low-Low" = "#2166ac", "High-Low" = "#ef8a62", "Low-High" = "#67a9cf", "Not significant" = "grey88")
hot_colours <- c("Hot spot" = "#b2182b", "Cold spot" = "#2166ac", "Not significant" = "grey88")

map_categorical <- function(sf_obj, value, colours, title, legend, subtitle = NULL, caption = NULL) {
  sf_obj$.v <- factor(sf_obj[[value]], levels = names(colours))
  ggplot2::ggplot(sf_obj) + ggplot2::geom_sf(ggplot2::aes(fill = .v), colour = "grey30", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = colours, name = legend, drop = FALSE, na.value = "white") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) + theme_map()
}

#' Interactive multi-layer map (leaflet). Needs the 'leaflet' and 'htmlwidgets' packages.
#' `layers` is a named list: each element is list(value = column, label = legend title, type = "continuous"|"categorical", colours = optional named vector).
#' Popups show the county, estimate with CI, sample size and cluster classes when present.
interactive_map <- function(sf_obj, layers, file, title = "County map") {
  if (!requireNamespace("leaflet", quietly = TRUE) || !requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("Package 'leaflet' (and 'htmlwidgets') not installed: interactive map skipped. install.packages('leaflet')")
    return(invisible(NULL))
  }
  popup <- paste0("<b>", sf_obj$county_boundary, "</b>",
    if ("pct" %in% names(sf_obj)) sprintf("<br>Adequate ANC: %.1f%% (95%% CI %.1f to %.1f)", sf_obj$pct, sf_obj$low, sf_obj$high) else "",
    if ("n" %in% names(sf_obj)) paste0("<br>Women: ", sf_obj$n) else "",
    if ("small_n" %in% names(sf_obj)) ifelse(sf_obj$small_n, "<br><i>Small sample: imprecise</i>", "") else "",
    if ("cluster" %in% names(sf_obj)) paste0("<br>LISA: ", sf_obj$cluster) else "",
    if ("hotspot" %in% names(sf_obj)) paste0("<br>Gi*: ", sf_obj$hotspot) else "")
  m <- leaflet::leaflet(sf_obj, options = leaflet::leafletOptions(minZoom = 5)) |> leaflet::addProviderTiles("CartoDB.Positron")
  groups <- character()
  for (nm in names(layers)) {
    L <- layers[[nm]]; groups <- c(groups, nm)
    if (L$type == "continuous") {
      pal <- leaflet::colorQuantile("viridis", sf_obj[[L$value]], n = 5, reverse = TRUE)
      m <- m |> leaflet::addPolygons(fillColor = ~pal(sf_obj[[L$value]]), fillOpacity = 0.85, color = "#444444", weight = 0.8, popup = popup, group = nm,
                                     highlightOptions = leaflet::highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) |>
        leaflet::addLegend("bottomright", pal = pal, values = sf_obj[[L$value]], title = L$label, group = nm, opacity = 0.9)
    } else {
      cols <- L$colours; pal <- leaflet::colorFactor(unname(cols), domain = names(cols))
      m <- m |> leaflet::addPolygons(fillColor = pal(as.character(sf_obj[[L$value]])), fillOpacity = 0.85, color = "#444444", weight = 0.8, popup = popup, group = nm,
                                     highlightOptions = leaflet::highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) |>
        leaflet::addLegend("bottomleft", colors = unname(cols), labels = names(cols), title = L$label, group = nm, opacity = 0.9)
    }
  }
  m <- m |> leaflet::addLayersControl(baseGroups = groups, options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::hideGroup(groups[-1]) |> leaflet::addScaleBar("bottomleft")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch({ htmlwidgets::saveWidget(m, file, selfcontained = TRUE, title = title); TRUE },
                 error = function(e) { message("Self-contained save needs pandoc; saving with a support folder instead."); FALSE })
  if (!ok) htmlwidgets::saveWidget(m, file, selfcontained = FALSE, title = title)
  invisible(m)
}
