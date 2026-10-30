# Builds a small synthetic DHS-like file and a 12-county grid so the pipeline can be tested
# without DHS data. The numbers mean nothing; only the structure matters.
# Usage: Rscript tests/make_synthetic.R <output_dir>
out <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(out)) out <- tempdir()
dir.create(out, showWarnings = FALSE, recursive = TRUE); set.seed(1)
nm <- c("Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf", "Hotel", "India", "Juliet", "Kilo", "Lima")
n <- 4000; cty <- sample(1:12, n, TRUE); psu <- cty * 100 + sample(1:8, n, TRUE)
edu <- sample(0:3, n, TRUE, c(.1, .4, .35, .15)); wealth <- sample(1:5, n, TRUE); u <- rnorm(12, 0, .4)[cty]
eta <- -0.2 + .35 * edu + .2 * wealth + u + rnorm(n, 0, .2)
visits <- rbinom(n, 10, plogis(eta - .8))
d <- data.frame(v005 = round(runif(n, .4, 2) * 1e6), v021 = psu, v023 = cty %% 4 + 1,
  v024 = haven::labelled(cty, setNames(1:12, nm)), v025 = sample(1:2, n, TRUE), v012 = sample(15:49, n, TRUE),
  v106 = edu, v190 = wealth, v201 = rpois(n, 2.5), v502 = sample(0:1, n, TRUE, c(.3, .7)),
  v743a = ifelse(runif(n) < .7, sample(1:6, n, TRUE), NA), v467d = ifelse(runif(n) < .6, sample(1:2, n, TRUE), NA),
  v157 = sample(0:3, n, TRUE), v158 = sample(0:3, n, TRUE), v159 = sample(0:3, n, TRUE), v171a = sample(0:3, n, TRUE),
  v222 = sample(0:80, n, TRUE), m14_1 = visits, m13_1 = ifelse(visits > 0, sample(1:8, n, TRUE), NA))
d$v743a[d$v502 == 0] <- NA
haven::write_sav(d, file.path(out, "synthetic_IR.sav"))
sq <- function(i, j) sf::st_polygon(list(rbind(c(j, i), c(j + 1, i), c(j + 1, i + 1), c(j, i + 1), c(j, i))))
g <- sf::st_sf(county = nm, geometry = sf::st_sfc(lapply(0:11, function(k) sq(k %/% 4, k %% 4)), crs = 4326))
sf::st_write(g, file.path(out, "synthetic_counties.geojson"), delete_dsn = TRUE, quiet = TRUE)
cat("Synthetic files written to", out, "\n")
