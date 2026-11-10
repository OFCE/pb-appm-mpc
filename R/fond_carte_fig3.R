## Fond de carte monde pour la fig. 3 (GISCO, 1:20M), sauvegardé pour un rendu hors-ligne.
## La France est réduite à sa partie métropolitaine (comme sur la figure d'origine).
library(sf)
monde <- giscoR::gisco_get_countries(resolution = "20", year = "2020")
monde <- monde[monde$ISO3_CODE != "ATA", c("ISO3_CODE", "NAME_FREN")]
monde <- st_make_valid(monde)
sf_use_s2(FALSE)
fra <- monde[monde$ISO3_CODE == "FRA", ]
fra_poly <- st_cast(fra, "POLYGON", warn = FALSE)
cent <- st_coordinates(st_centroid(st_geometry(fra_poly)))
metro <- fra_poly[cent[, 1] > -6 & cent[, 1] < 10 & cent[, 2] > 41 & cent[, 2] < 52, ]
st_geometry(fra) <- st_union(st_geometry(metro))
monde <- rbind(monde[monde$ISO3_CODE != "FRA", ], fra)
sf_use_s2(TRUE)
st_write(monde, "data/fig3_fond_carte.gpkg", delete_dsn = TRUE, quiet = TRUE)
