#' gg_map
#'
#' @description Create a static projected map as a ggplot2 object using sf and
#'   Natural Earth basemap data. Supports a wide range of projections and
#'   flexible centring. The returned object can be extended with any ggplot2
#'   layer.
#'
#' @param proj projection to use. Either a named shortcut (see Details) or any
#'   string accepted by \code{sf::st_crs()}, e.g. a PROJ string such as
#'   \code{"+proj=moll +lon_0=0"} or an EPSG code such as \code{"EPSG:4326"}.
#' @param focal.lat central latitude for projections that support it (default
#'   0). Affects azimuthal and perspective projections.
#' @param focal.lon central longitude for the projection (default 0).
#' @param lat.range latitude window to display as \code{c(min, max)}, in
#'   degrees (default \code{c(-90, 90)}).
#' @param lon.range longitude window to display as \code{c(min, max)}, in
#'   degrees (default \code{c(-180, 180)}).
#' @param land.colour fill colour for land (default \code{"grey80"}).
#' @param ocean.colour background colour for the ocean (default
#'   \code{"#d8e8f0"}).
#' @param border.colour colour for country borders (default \code{"grey40"}).
#' @param resolution resolution of the Natural Earth basemap: \code{110}
#'   (coarse, ~1:110m, default), \code{50} (medium), or \code{10} (fine).
#'   Requires the \pkg{rnaturalearth} package.
#' @param graticule logical; draw a graticule (lat/lon grid)? Default
#'   \code{TRUE}.
#' @param graticule.colour colour for graticule lines (default
#'   \code{"grey70"}).
#'
#' @details
#' Named projections accepted by \code{proj}:
#'
#' Projections centred on \code{focal.lon} only: \code{"mollweide"},
#' \code{"robinson"}, \code{"eckert4"}, \code{"natural earth"},
#' \code{"kavrayskiy7"}, \code{"miller"}, \code{"winkel tripel"},
#' \code{"aitoff"}, \code{"sinusoidal"}, \code{"hammer"},
#' \code{"equirectangular"}, \code{"mercator"}.
#'
#' Projections centred on both \code{focal.lat} and \code{focal.lon}:
#' \code{"azimuthal equal area"}, \code{"azimuthal equidistant"},
#' \code{"orthographic"}, \code{"stereographic"}, \code{"gnomonic"},
#' \code{"conic equal area"}, \code{"conic equidistant"},
#' \code{"transverse mercator"}.
#'
#' Any other value for \code{proj} is passed directly to \code{sf::st_crs()}.
#'
#' @return A \code{ggplot2} object. Add data or style layers with \code{+} as
#'   usual. Non-sf layers using \code{x}/\code{y} aesthetics (e.g.
#'   \code{geom_point}) are treated as lon/lat in WGS84 and projected
#'   automatically.
#'
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_void theme element_rect
#'   element_blank element_line
#' @importFrom sf st_crs
#' @export
#' @examples
#' # Mollweide (default)
#' gg_map()
#'
#' # Robinson centred on the Pacific
#' gg_map(proj = "robinson", focal.lon = 150)
#'
#' # Azimuthal equal area, North Pole
#' gg_map(proj = "azimuthal equal area", focal.lat = 90, lat.range = c(0, 90))
#'
#' # Orthographic view from above Europe
#' gg_map(proj = "orthographic", focal.lat = 50, focal.lon = 10)
#'
#' # Regional equirectangular map of the North Atlantic
#' gg_map(proj = "equirectangular",
#'        lat.range = c(20, 80), lon.range = c(-80, 40))
#'
#' # Add point data (x/y treated as lon/lat automatically)
#' dat <- data.frame(lat = c(15, 45, -30), lon = c(22, -90, 140))
#' gg_map() +
#'   ggplot2::geom_point(data = dat, ggplot2::aes(x = lon, y = lat),
#'                       colour = "red", size = 2)
gg_map <- function(
    proj           = "mollweide",
    focal.lat      = 0,
    focal.lon      = 0,
    lat.range      = c(-90, 90),
    lon.range      = c(-180, 180),
    land.colour    = "grey80",
    ocean.colour   = "#d8e8f0",
    border.colour  = "grey40",
    resolution     = 110,
    graticule      = TRUE,
    graticule.colour = "grey70") {

  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("Package 'rnaturalearth' is required. Install with: ",
         "install.packages('rnaturalearth')")
  }
  if (!resolution %in% c(10, 50, 110)) {
    stop("resolution must be one of 10, 50, or 110")
  }

  crs   <- .gg_map_crs(proj, focal.lat, focal.lon)
  world <- rnaturalearth::ne_countries(scale = resolution, returnclass = "sf")

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world,
                     fill     = land.colour,
                     colour   = border.colour,
                     linewidth = 0.3) +
    ggplot2::coord_sf(crs         = crs,
                      xlim        = lon.range,
                      ylim        = lat.range,
                      default_crs = sf::st_crs(4326),
                      expand      = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = ocean.colour, colour = NA),
      panel.border     = ggplot2::element_rect(fill = NA, colour = "grey20",
                                               linewidth = 0.5),
      panel.grid.major = if (graticule) {
        ggplot2::element_line(colour   = graticule.colour,
                              linewidth = 0.3,
                              linetype = "dashed")
      } else {
        ggplot2::element_blank()
      }
    )

  p
}


# Internal: build an sf CRS from a named projection + focal coordinates.
# Returns sf::st_crs() object. If proj is not a known name it is passed
# directly to sf::st_crs().
.gg_map_crs <- function(proj, focal.lat, focal.lon) {

  # PROJ string templates. %1$s = focal.lat, %2$s = focal.lon
  templates <- c(
    "mollweide"              = "+proj=moll    +lon_0=%2$s",
    "robinson"               = "+proj=robin   +lon_0=%2$s",
    "eckert4"                = "+proj=eck4    +lon_0=%2$s",
    "natural earth"          = "+proj=natearth +lon_0=%2$s",
    "kavrayskiy7"            = "+proj=kav7    +lon_0=%2$s",
    "miller"                 = "+proj=mill    +lon_0=%2$s",
    "winkel tripel"          = "+proj=wintri  +lon_0=%2$s",
    "aitoff"                 = "+proj=aitoff  +lon_0=%2$s",
    "sinusoidal"             = "+proj=sinu    +lon_0=%2$s",
    "hammer"                 = "+proj=hammer  +lon_0=%2$s",
    "equirectangular"        = "+proj=eqc     +lon_0=%2$s",
    "mercator"               = "+proj=merc    +lon_0=%2$s",
    "azimuthal equal area"   = "+proj=laea    +lat_0=%1$s +lon_0=%2$s",
    "azimuthal equidistant"  = "+proj=aeqd    +lat_0=%1$s +lon_0=%2$s",
    "orthographic"           = "+proj=ortho   +lat_0=%1$s +lon_0=%2$s",
    "stereographic"          = "+proj=stere   +lat_0=%1$s +lon_0=%2$s",
    "gnomonic"               = "+proj=gnom    +lat_0=%1$s +lon_0=%2$s",
    "conic equal area"       = "+proj=aea     +lat_0=%1$s +lon_0=%2$s",
    "conic equidistant"      = "+proj=eqdc    +lat_0=%1$s +lon_0=%2$s",
    "transverse mercator"    = "+proj=tmerc   +lat_0=%1$s +lon_0=%2$s"
  )

  tmpl <- templates[proj]
  if (is.na(tmpl)) {
    return(sf::st_crs(proj))
  }

  sf::st_crs(sprintf(tmpl, focal.lat, focal.lon))
}
