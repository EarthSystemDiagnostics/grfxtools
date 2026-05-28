#' gg_map_rast
#'
#' @description Raster-based alternative to \code{\link{gg_map}}. Land polygons
#'   are rasterised in WGS84 and the resulting grid is reprojected with
#'   \code{terra::project()}, so there are no polygon-edge artefacts at the
#'   antimeridian regardless of the central meridian chosen. The returned object
#'   uses the target projection's coordinate units on both axes; see Details for
#'   how to overlay data.
#'
#' @param proj projection to use. Either a named shortcut (same list as
#'   \code{\link{gg_map}}) or any string accepted by \code{sf::st_crs()}.
#' @param focal.lat central latitude (default 0).
#' @param focal.lon central longitude (default 0).
#' @param lat.range latitude window as \code{c(min, max)} in degrees (default
#'   \code{c(-90, 90)}).
#' @param lon.range longitude window as \code{c(min, max)} in degrees (default
#'   \code{c(-180, 180)}).
#' @param land.colour fill colour for land (default \code{"grey80"}).
#' @param ocean.colour background colour for ocean (default \code{"#d8e8f0"}).
#' @param resolution resolution of the Natural Earth source polygons: \code{110}
#'   (coarse, default), \code{50} (medium), or \code{10} (fine).
#' @param nx number of columns in the WGS84 source raster (default \code{1000};
#'   increase for sharper coastlines at the cost of render time).
#' @param graticule logical; draw a lat/lon graticule? Default \code{TRUE}.
#' @param graticule.colour colour for graticule lines (default \code{"grey70"}).
#'
#' @details
#' Because the plot is built in the projected coordinate system (typically
#' metres), data layers added with \code{+} must supply coordinates in the same
#' system. Project lon/lat point data like this:
#'
#' \preformatted{
#' crs <- sf::st_crs("+proj=robin +lon_0=180")
#' pts_proj <- sf::st_coordinates(
#'   sf::st_transform(
#'     sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326), crs))
#' }
#'
#' @return A \code{ggplot2} object with \code{coord_equal()}.
#'
#' @importFrom ggplot2 ggplot geom_raster geom_path coord_equal theme_void
#'   theme element_rect element_blank element_line aes
#' @importFrom sf st_crs st_graticule st_transform st_coordinates
#' @export
#' @examples
#' # Mollweide (default)
#' gg_map_rast()
#'
#' # Pacific-centred Robinson, tropical belt — no antimeridian artefacts
#' gg_map_rast(proj = "robinson", focal.lon = 180, lat.range = c(-35, 35))
#'
#' # Azimuthal equal area, North Pole
#' gg_map_rast(proj = "azimuthal equal area", focal.lat = 90,
#'             lat.range = c(0, 90))
#'
#' # Adding coral site points (project lon/lat to the map CRS first)
#' \donttest{
#' coral_sites <- data.frame(
#'   lon = c(147, -65,  37,  73, -157, 134,  55, -175),
#'   lat = c(-18,  15,  20,   4,   20,   7, -10,  -18)
#' )
#' rob180 <- sf::st_crs("+proj=robin +lon_0=180")
#' pts_proj <- as.data.frame(sf::st_coordinates(
#'   sf::st_transform(
#'     sf::st_as_sf(coral_sites, coords = c("lon", "lat"), crs = 4326),
#'     rob180)))
#' names(pts_proj) <- c("x", "y")
#'
#' gg_map_rast(proj = "robinson", focal.lon = 180, lat.range = c(-35, 35)) +
#'   ggplot2::geom_point(data = pts_proj,
#'                       ggplot2::aes(x = x, y = y),
#'                       colour = "coral", size = 2)
#' }
gg_map_rast <- function(
    proj             = "mollweide",
    focal.lat        = 0,
    focal.lon        = 0,
    lat.range        = c(-90, 90),
    lon.range        = c(-180, 180),
    land.colour      = "grey80",
    ocean.colour     = "#d8e8f0",
    resolution       = 110,
    nx               = 1000,
    graticule        = TRUE,
    graticule.colour = "grey70") {

  if (!requireNamespace("rnaturalearth", quietly = TRUE))
    stop("Package 'rnaturalearth' is required. Install with: ",
         "install.packages('rnaturalearth')")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required. Install with: ",
         "install.packages('terra')")
  if (!resolution %in% c(10, 50, 110))
    stop("resolution must be one of 10, 50, or 110")

  crs_obj <- .gg_map_crs(proj, focal.lat, focal.lon)

  # --- 1. Rasterize land in WGS84 ----------------------------------------
  world_vect <- terra::vect(
    rnaturalearth::ne_countries(scale = resolution, returnclass = "sf")
  )

  ny      <- round(nx * diff(lat.range) / diff(range(c(-180, 180))))
  r_src   <- terra::rast(
    xmin  = max(lon.range[1], -180), xmax = min(lon.range[2], 180),
    ymin  = lat.range[1],            ymax  = lat.range[2],
    nrows = max(ny, 10L),            ncols = nx,
    crs   = "EPSG:4326"
  )
  land_src <- terra::rasterize(world_vect, r_src, field = 1, background = NA)

  # --- 2. Reproject raster ------------------------------------------------
  # nearest-neighbor preserves the binary land/ocean values; no interpolation
  # artefacts at coastlines.
  land_proj <- terra::project(land_src, crs_obj$wkt, method = "near")

  xlim_p <- c(terra::xmin(land_proj), terra::xmax(land_proj))
  ylim_p <- c(terra::ymin(land_proj), terra::ymax(land_proj))

  # --- 3. Data frame (land cells only) ------------------------------------
  land_df        <- as.data.frame(land_proj, xy = TRUE, na.rm = TRUE)
  names(land_df) <- c("x", "y", "land")

  # --- 4. Base plot -------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = land_df,
                          ggplot2::aes(x = .data$x, y = .data$y),
                          fill = land.colour) +
    ggplot2::coord_equal(xlim = xlim_p, ylim = ylim_p, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = ocean.colour, colour = NA),
      panel.border     = ggplot2::element_rect(fill = NA, colour = "grey20",
                                               linewidth = 0.5)
    )

  # --- 5. Graticule -------------------------------------------------------
  if (graticule) {
    grat_sf     <- sf::st_graticule(lat = seq(-90, 90, 30),
                                     lon = seq(-180, 180, 45),
                                     crs = sf::st_crs(4326))
    grat_proj   <- sf::st_transform(grat_sf, crs_obj)
    grat_coords <- sf::st_coordinates(grat_proj)
    level_cols <- grep("^L", colnames(grat_coords), value = TRUE)
    grat_df <- data.frame(
      x     = grat_coords[, "X"],
      y     = grat_coords[, "Y"],
      group = apply(grat_coords[, level_cols, drop = FALSE], 1,
                    paste, collapse = "-")
    )
    p <- p + ggplot2::geom_path(
      data      = grat_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
      colour    = graticule.colour,
      linewidth = 0.3,
      linetype  = "dashed",
      na.rm     = TRUE
    )
  }

  p
}
