# create basemaps

ne_land_50 <- rnaturalearth::ne_download(scale = 50, type = "land",
                                         category = "physical",
                                         returnclass = "sp")
ne_land_50_political <- rnaturalearth::ne_countries(scale = 50,
                                                    returnclass = "sp")

usethis::use_data(ne_land_50, ne_land_50_political,
                  internal = TRUE, overwrite = TRUE)



# also add to exported objects
# usethis::use_data(ne_land_50, ne_land_50_political, overwrite = TRUE)
