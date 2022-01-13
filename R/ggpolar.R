#' Nicely formatted polar ggplot
#'
#' Produce nicely formatted maps of the Arctic or Antarctic in polar
#' projection. Also works for longitudinal segments, e.g. just Greenland.
#'
#' @param pole character; which pole: north ("N") or south ("S")? Note that
#'   full-longitude south polar plots only include the Antarctic main land,
#'   irrespective of the latitude setting.
#' @param data.layer optional ggplot2 layer of data onto which the polar map
#'   shall be plotted. Defaults to \code{NULL} which only plots the map.
#' @param max.lat maximum latitude in degree.
#' @param min.lat minimum latitude in degree.
#' @param max.lon maximum longitude in degree.
#' @param min.lon minimum longitude in degree.
#' @param longitude.spacing interval between longitude axis tick marks.
#' @param n.lat.labels approximate number of latitude tick marks.
#' @param plot.political.boundaries logical; should political (country)
#'   boundaries be plotted? Defaults to FALSE.
#' @param land.fill.colour colour to shade the land (default "Grey").
#' @param land.outline.colour colour for the land surface outlines (default
#'   "Black"); set to the same colour as \code{land.fill.colour} to hide
#'   them. This parameter also controls the colours of the political boundaries,
#'   if they are switched on.
#' @param rotate logical; if plotting a segment of < 360 degrees longitude,
#'   rotate the plot so that north is up (or south is down) as seen from the
#'   mean longitude of the segment.
#' @param nearest.x.degrees round latitude tickmarks to how many degrees?
#' @param lat.ax.vals manually set the latitude axis values where to plot
#'   latitude labels and lines. This overrides the automatic setting by
#'   \code{n.lat.labels} and \code{x.nearest.degress}, while the default
#'   \code{NULL} means to use the automatic setting.
#' @param long.ax.vals manually set the longitude axis values where to plot
#'   longitude labels and lines. This overrides the automatic setting by
#'   \code{longitude.spacing}, while the default \code{NULL} means to use the
#'   automatic setting.
#' @param plt.lat.axes logical; shall latitude axes be plotted?
#' @param plt.lat.labels logical; shall latitude labels be plotted? Per default
#'   set to the value of \code{plt.lat.axes}.
#' @param plt.lon.axes logical; shall longitude axes be plotted?
#' @param plt.lon.labels logical; shall longitude labels be plotted? Per default
#'   set to the value of \code{plt.lon.axes}.
#' @param f.long.label.ticks fraction of the plotted latitude axis range by
#'   which the longitude ticksmarks extend behind the outer latitude axis,
#'   i.e. the minimum (maximum) latitude for north (south) polar plots.
#' @param f.long.label.pos fraction of the plotted latitude axis range by
#'   which the longitude labels are offset from the outer latitude axis.
#' @param rotate.long.labels logical; controls whether the longitude axis labels
#'   are rotated according to their value and the setting of \code{rotate} (the
#'   default) or not.
#' @param lat.ax.labs.pos longitudinal positions of the latitude axis
#'   labels. Per default, the labels are placed at 0 E (180 W) for north (south)
#'   polar plots or at the mean longitude of a segment; use this parameter to
#'   override the default setting.
#' @param ax.labs.size size of latitude and longitude axis labels.
#' @param size.outer size of the outer (and potential inner) longitude circle
#'   and, if plotting a segment, of the outer latitude lines.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of \code{"on"} (the default) means yes, and a setting of
#'   \code{"off"} means no. For details, please see
#'   \code{\link[ggplot2]{coord_cartesian}}.
#' @importFrom rlang .data
#' @author Andrew Dolman <andrew.dolman@awi.de> with some modifications by
#'   Thomas Münch.
#' @source Code adapted from Mikey Harper's plot; see
#'   https://stackoverflow.com/a/49084793.
#'
#'   Coastlines and political boundaries for complete north polar plots, and for
#'   north and south polar segment plots, have been obtained from the
#'   rnaturalearth package:
#'   https://cran.r-project.org/web/packages/rnaturalearth/index.html
#'
#'   Complete south polar plots use ggplot2 data.
#' @examples
#'
#' # full north polar plot
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4)
#'
#' # with political boundaries
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4,
#'         plot.political.boundaries = TRUE)
#'
#' # hide land surface outlines
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4,
#'         land.outline.colour = "Grey")
#'
#' # full south polar plot
#' ggpolar(pole = "S", max.lat = -60, min.lat = -90)
#'
#' # north polar segment plot with adjusted longitude and latitude labelling
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'         max.lon = 0, min.lon = -80,
#'         longitude.spacing = 15, n.lat.labels = 5)
#'
#' # rotate the segment
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'         max.lon = 0, min.lon = -80,
#'         longitude.spacing = 15, n.lat.labels = 5,
#'         rotate = TRUE)
#'
#' # south polar segment plot
#' ggpolar(pole = "S", max.lat = -55, min.lat = -90,
#'         max.lon = 80, min.lon = -20,
#'         longitude.spacing = 30, rotate = TRUE)
#'
#' # it is also possible to plot a ring segment;
#' # additionally, there are various options to cuzstomize position and look of
#' # axis labels etc., with a few examples shown here
#' ggpolar(pole = "N", max.lat = 85, min.lat = 57.5,
#'   max.lon = -10, min.lon = -75, rotate = TRUE,
#'   lat.ax.vals = c(60, 70, 80),   # explicitly specify latitude labels
#'   long.ax.vals = -c(20, 40, 60), # explicitly specify longitude labels
#'   f.long.label.ticks = Inf,      # don't draw longitude lines beyond segment
#'   f.long.label.pos = 15,         # adjust label position from segment outline
#'   rotate.long.labels = FALSE,    # don't rotate longitude labels
#'   lat.ax.labs.pos = -75 - c(3.5, 5, 10), # move latitude labels to left side
#'   ax.labs.size = 4.75)                   # larger labels
#'
#' # ---------------------------------------------------------------------------
#'
#' # if you want to add points or a spatial field of data to a ggpolar plot, it
#' # can be convenient to create that plot separately and then provide it as an
#' # argument to ggpolar, so that it can be added to the polar plot internally:
#'
#' library(ggplot2)
#'
#' # create some dummy data
#' nx <- 360 / 5
#' ny <- 30 / 5
#'
#' lon <- seq(0, 355, length.out = nx)
#' lat <- seq(-65, -90, length.out = ny)
#'
#' df <- data.frame(lon = rep(lon, times = ny), lat = rep(lat, each = nx),
#'                  dat = runif(n = nx * ny, min = -1, max = 1))
#'
#' # Colorbrewer2 colour scales for plotting the "correlation" data can be
#' # obtained directly with the respective grfxtools function
#' colour.scale <- ColorPal("RdBu", rev = TRUE)
#'
#' # create ggplot for data
#' p <- ggplot() +
#'   geom_tile(aes(x = lon, y = lat, fill = dat),
#'             data = df, colour = "transparent") +
#'   scale_fill_gradientn(colours = colour.scale,
#'                        limits = c(-1, 1),
#'                        name = "Correlation") +
#'   theme(legend.key.height = unit(0.75, units = "inches"),
#'         legend.text = element_text(size = 18),
#'         legend.title = element_text(size = 18),
#'         text = element_text(size = 18))
#'
#' # plot data projected on south polar map
#' ggpolar(pole = "S", data.layer = p,
#'         max.lat = -60, min.lat = -90, n.lat.labels = 3,
#'         longitude.spacing = 45,
#'         land.fill.colour = "transparent")
#'
#' # ---------------------------------------------------------------------------
#'
#' \dontrun{
#' ggpolar(pole = "W", max.lat = -55, min.lat = -90)
#' ggpolar(pole = "S", max.lat = 90, min.lat = 55)
#' ggpolar(pole = "S", max.lat = -90, min.lat = -55)
#' }
#' @export
ggpolar <- function(pole = c("N", "S"), data.layer = NULL, max.lat, min.lat,
                    max.lon = 180, min.lon = -180, longitude.spacing = 60,
                    n.lat.labels = 4, plot.political.boundaries = FALSE,
                    land.fill.colour = "Grey", land.outline.colour = "Black",
                    rotate = FALSE, nearest.x.degrees = 5,
                    lat.ax.vals = NULL, long.ax.vals = NULL,
                    plt.lat.axes = TRUE, plt.lat.labels = plt.lat.axes,
                    plt.lon.axes = TRUE, plt.lon.labels = plt.lon.axes,
                    f.long.label.ticks = 20, f.long.label.pos = 7,
                    rotate.long.labels = TRUE, lat.ax.labs.pos = NULL,
                    ax.labs.size = 4, size.outer = 1, clip = "on") {

  pole <- match.arg(pole, choices = c("N", "S"))

  if (pole == "N" & max.lat < 0) stop("If pole == N, max.lat should be positive")
  if (pole == "S" & min.lat > 0) stop("If pole == S, min.lat should be negative")

  # degree symbol
  degree.sym <- "\u00b0"

  stopifnot(max.lat > min.lat)
  stopifnot(max.lon <= 180 & min.lon >= -180 & max.lat <= 90 & min.lat >= -90)

  is.segment <- (min.lon == -180 & max.lon == 180) == FALSE

  mean.lon <- mean(c(min.lon, max.lon))

  # if not a segment
  if (!is.segment) {
    mean.lon <- ifelse(pole == "N", 0, 180)
  }

  rotate.to <- 0
  if (rotate & is.segment) {rotate.to <- mean.lon}

  lat.range <- abs(max.lat - min.lat)
  d.lat.ax.vals <- nearest.x.degrees *
    round((max.lat - min.lat)/n.lat.labels/nearest.x.degrees)

  # Hemisphere specific values
  if (pole == "N") {

    if (!length(lat.ax.vals)) {
      lat.ax.vals <- seq(min.lat + d.lat.ax.vals, max.lat, by = d.lat.ax.vals)
    }

    outer.lat.ax.val <- min.lat
    inner.lat.ax.val <- ifelse(max.lat == 90, NA, max.lat)
    long.lab.pos.1 <- outer.lat.ax.val - (lat.range / f.long.label.ticks)
    long.lab.pos.2 <- outer.lat.ax.val - (lat.range / f.long.label.pos)

    long.line.strt <- max.lat
    long.line.end <- long.lab.pos.1

  } else if (pole == "S") {

    if (!length(lat.ax.vals)) {
      lat.ax.vals <- seq(max.lat - d.lat.ax.vals, min.lat, by = -d.lat.ax.vals)
    }

    outer.lat.ax.val <- max.lat
    inner.lat.ax.val <- ifelse(min.lat == -90, NA, min.lat)
    long.lab.pos.1 <- outer.lat.ax.val + (lat.range / f.long.label.ticks)
    long.lab.pos.2 <- outer.lat.ax.val + (lat.range / f.long.label.pos)

    long.line.strt <- long.lab.pos.1
    long.line.end <- min.lat
  }

  lat.ax.vals <- lat.ax.vals[abs(lat.ax.vals) != 90]
  lat.ax.labs <- paste0(ifelse(lat.ax.vals < 0, -lat.ax.vals, lat.ax.vals),
                        degree.sym, ifelse(lat.ax.vals < 0, " S", " N"))

  # Define the x axes required
  if (!length(long.ax.vals)) {
    long.ax.vals <- seq(min.lon, max.lon, by = longitude.spacing)
  }

  if (!is.segment) {
    long.ax.vals <- long.ax.vals[long.ax.vals != -180]
  }

  long.ax.labs <- paste0(abs(long.ax.vals), degree.sym,
                         ifelse(long.ax.vals <= 0, " W", " E"))
  long.ax.labs[long.ax.vals == 0] <- paste0(0, degree.sym)
  long.ax.labs[long.ax.vals == 180] <- paste0(180, degree.sym, " W")

  lat.lines <- expand.grid(long = min.lon : max.lon, lat = lat.ax.vals)

  # Define rotation angle for longitude labels
  if (rotate.long.labels) {

    if (pole == "N") {

      long.ax.lab.rotation <- long.ax.vals - 180 - rotate.to
      if (is.segment) long.ax.lab.rotation <- long.ax.lab.rotation + 180

    } else {

      long.ax.lab.rotation <- -long.ax.vals + rotate.to
    }

  } else {

    long.ax.lab.rotation <- 0

  }

  # Get map outline and crop
  if (is.segment | (pole == "N")) {

    if (plot.political.boundaries) {

      map.outline <- raster::crop(ne_land_50_political,
                                  raster::extent(min.lon, max.lon,
                                                 min.lat, max.lat),
                                  snap = "in")

    } else {

      map.outline <- raster::crop(ne_land_50,
                                  raster::extent(min.lon, max.lon,
                                                 min.lat, max.lat),
                                  snap = "in")

    }

  } else {

    # Use different map source for south polar full longitude plot
    # to circumvent buggy line at 180 deg W
    map.outline <- ggplot2::map_data("world", "Antarctica")
    i <- which(map.outline$lat >= min.lat & map.outline$lat <= max.lat)
    map.outline <- map.outline[i, ]

  }

  if (!length(data.layer)) {
    p <- ggplot2::ggplot()
  } else {
    p <- data.layer
  }

  p <- p +

    # Plot map outline and project to polar coordinates
    ggplot2::geom_polygon(data = map.outline,
                          ggplot2::aes(x = .data$long, y = .data$lat,
                                       group = .data$group),
                          fill = land.fill.colour,
                          colour = land.outline.colour) +

    ggplot2::coord_map("ortho",
                       orientation = c(ifelse(pole == "N", 90, -90),
                                       rotate.to, 0),
                       xlim = c(min.lon, max.lon), clip = clip
                       ) +

    # Remove axes and labels
    ggplot2::scale_x_continuous("", breaks = NULL) +
    ggplot2::scale_y_continuous("", breaks = NULL) +

    # Outer latitude axis
    ggplot2::geom_line(
      ggplot2::aes(y = outer.lat.ax.val, x = min.lon : max.lon),
      size = size.outer, colour = "black")

    # Inner latitude axis
    if (!is.na(inner.lat.ax.val)) {
      p <- p + ggplot2::geom_line(
                 ggplot2::aes(y = inner.lat.ax.val, x = min.lon : max.lon),
                 size = size.outer, colour = "black")
    }

    # Change theme to remove panel backgound
    p <- p + ggplot2::theme(panel.background = ggplot2::element_blank())

  # Add axes and labels

  # Lat axis lines
  if (plt.lat.axes) {
    p <- p + ggplot2::geom_line(data = lat.lines,
                                ggplot2::aes(y = .data$lat, x = .data$long,
                                             group = .data$lat),
                                size = 0.25, linetype = "dashed",
                                colour = "black")
  }
  # Lat axis labels
  if (plt.lat.labels) {

    if (!length(lat.ax.labs.pos)) {
      lat.ax.labs.pos <- mean.lon
    }

    p <- p + ggplot2::geom_label(
               ggplot2::aes(x = lat.ax.labs.pos, y = lat.ax.vals,
                            label = lat.ax.labs),
               size = ax.labs.size,
               alpha = 0.75, label.size = 0)
  }

  # Longitude axis lines
  if (plt.lon.axes) {

    p <- p + ggplot2::geom_segment(
               ggplot2::aes(y = long.line.strt, yend = long.line.end,
                            x = long.ax.vals, xend = long.ax.vals),
               linetype = "dashed", colour = "black", size = 0.25)
  }
  # Longitude axis labels
  if (plt.lon.labels) {

    p <- p + ggplot2::geom_text(
               ggplot2::aes(x = long.ax.vals,
                            y = long.lab.pos.2,
                            label = long.ax.labs,
                            angle = long.ax.lab.rotation),
               size = ax.labs.size)
  }

  # If segment add lines to edge
  if (is.segment) {
    p <- p + ggplot2::geom_segment(
               ggplot2::aes(y = long.line.strt, yend = long.line.end,
                            x = c(min.lon, max.lon), xend = c(min.lon, max.lon)),
               colour = "black", size = size.outer)
  }

  p
}
