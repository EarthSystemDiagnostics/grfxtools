# create basemaps

ne_land_50 <- rnaturalearth::ne_download(scale = 50, type = "land", category = "physical", returnclass = "sp")

#maptools_wrld_simpl <- grfxtools:::maptools_wrld_simpl

maptools_wrld_simpl <- rnaturalearth::ne_countries(scale = 50, returnclass = "sp")


usethis::use_data(ne_land_50, maptools_wrld_simpl, internal = TRUE, overwrite = TRUE)
