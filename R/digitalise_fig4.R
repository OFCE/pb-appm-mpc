## Digitalisation de images/fig4_projets_mines_europe.png
## (Bastianin, Del Bo & Shamsudin 2025, d'après Owen, Lèbre & Kemp 2022).
## Les données sources ne sont pas publiques : on extrait les points rouges
## de l'image et on les géoréférence à partir du graticule (Web Mercator,
## méridiens tous les 15°, parallèles tous les 10°).
## Sorties : data/fig4_projets_mines.csv et data/fig4_fond_carte.gpkg

library(png)
library(sf)
library(igraph)

im <- readPNG("images/fig4_projets_mines_europe.png")
r <- im[, , 1]; g <- im[, , 2]; b <- im[, , 3]

## 1. Pixels rouges -> composantes connexes (8-voisinage) ----
red <- r > 0.6 & g < 0.45 & b < 0.45
idx <- which(red, arr.ind = TRUE)          # colonnes : row (y), col (x)
key <- function(y, x) paste(y, x)
id <- seq_len(nrow(idx)); names(id) <- key(idx[, 1], idx[, 2])
edges <- do.call(rbind, lapply(list(c(0, 1), c(1, 0), c(1, 1), c(1, -1)), function(d) {
  nb <- id[key(idx[, 1] + d[1], idx[, 2] + d[2])]
  ok <- !is.na(nb)
  cbind(id[ok], nb[ok])
}))
gr <- make_empty_graph(n = length(id), directed = FALSE) |> add_edges(t(edges))
comp <- components(gr)$membership

## 2. Taille d'un point isolé, puis découpage des amas par k-means ----
taille <- tabulate(comp)
comp_ok <- which(taille >= 15)              # élimine le bruit d'anticrénelage
aire_pt <- median(taille[comp_ok])
set.seed(1)
pts <- do.call(rbind, lapply(comp_ok, function(k) {
  p <- idx[comp == k, , drop = FALSE]
  n <- max(1, round(nrow(p) / aire_pt))
  if (n == 1) return(data.frame(px = mean(p[, 2]), py = mean(p[, 1])))
  km <- kmeans(p[, 2:1], centers = n, nstart = 20)
  data.frame(px = km$centers[, 1], py = km$centers[, 2])
}))

## 3. Géoréférencement : ajustement linéaire pixel -> Web Mercator ----
merc <- function(lon, lat) st_coordinates(st_transform(
  st_sfc(st_point(c(lon, lat)), crs = 4326), 3857))
grille_x <- data.frame(px  = c(53.5, 275.5, 498, 720.5, 942.5, 1165, 1387.5, 1609.5, 1832),
                       lon = seq(-75, 45, by = 15))
grille_y <- data.frame(py  = c(1447.5, 1266, 1055.5, 795.5, 440.5),
                       lat = seq(30, 70, by = 10))
grille_x$X <- sapply(grille_x$lon, \(l) merc(l, 0)[1])
grille_y$Y <- sapply(grille_y$lat, \(l) merc(0, l)[2])
fit_x <- lm(X ~ px, grille_x); fit_y <- lm(Y ~ py, grille_y)
cat("Résidus max (px) :",
    max(abs(resid(fit_x))) / abs(coef(fit_x)[2]),
    max(abs(resid(fit_y))) / abs(coef(fit_y)[2]), "\n")

pts$X <- predict(fit_x, pts); pts$Y <- predict(fit_y, pts)
projets <- st_as_sf(pts, coords = c("X", "Y"), crs = 3857) |> st_transform(4326)
ll <- st_coordinates(projets)
out <- data.frame(lon = round(ll[, 1], 3), lat = round(ll[, 2], 3))
out <- out[order(out$lon, out$lat), ]
write.csv(out, "data/fig4_projets_mines.csv", row.names = FALSE)
cat("Points extraits :", nrow(out), " (aire d'un point :", aire_pt, "px)\n")

## 4. Fond de carte (GISCO) sauvegardé localement pour un rendu hors-ligne ----
pays <- giscoR::gisco_get_countries(resolution = "10", year = "2020")
sf_use_s2(FALSE)  # découpage planaire en lon/lat (avec s2, le bord du cadre serait géodésique)
pays <- st_crop(st_make_valid(pays), xmin = -90, xmax = 60, ymin = 20, ymax = 85)
sf_use_s2(TRUE)
pays <- pays[, c("ISO3_CODE", "NAME_FREN")]
st_write(pays, "data/fig4_fond_carte.gpkg", delete_dsn = TRUE, quiet = TRUE)
