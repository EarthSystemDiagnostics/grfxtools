#' Nicely formatted polar ggplot
#'
#' Produce nicely formatted maps of the Arctic or Antarctic in polar
#' projection. Also works for longitudinal segments, e.g. just Greenland.
#'
#' @param pole character; which pole: "N" or "S"?
#' @param max.lat maximum latitude.
#' @param min.lat minimum latitude.
#' @param max.lon maximum longitude.
#' @param min.lon minimum longitude.
#' @param longitude.spacing interval between longitude axis tick marks.
#' @param land.fill.colour colour to shade the land.
#' @param country.outline.colour colour for political boundaries (default
#'   "Black"); set to the same as \code{land.fill.colour} to hide them.
#' @param n.lat.labels approximate number of latitude tickmarks.
#' @param nearest.x.degrees round latitude tickmarks to how many degrees?
#' @param f.long.label.ticks fraction of the plotted latitude axis range by
#'   which the longitude ticksmarks extend behind the outer latitude axis,
#'   i.e. the minimum (maximum) latitude for north (south) polar plots.
#' @param f.long.label.pos fraction of the plotted latitude axis range by
#'   which the longitude labels are offset from the outer latitude axis.
#' @param lat.ax.vals manually set the latitude axis values where to plot
#'   latitude labels and lines. This overrides the automatic setting by
#'   \code{n.lat.labels} and \code{x.nearest.degress}, while the default
#'   \code{NULL} means to use the automatic setting.
#' @param long.ax.vals manually set the longitude axis values where to plot
#'   longitude labels and lines. This overrides the automatic setting by
#'   \code{longitude.spacing}, while the default \code{NULL} means to use the
#'   automatic setting.
#' @param rotate logical; if plotting a segment of < 360 degrees longitude,
#'   rotate the plot so that north is up (or south is down) as seen from the
#'   mean longitude of the segment.
#' @param size.outer size of the outer (and potential inner) longitude circle
#'   and, if plotting a segment, of the outer latitude lines.
#' @param plt.lat.axes logical; shall latitude axes be plotted?
#' @param plt.lat.labels logical; shall latitude labels be plotted? Per default
#'   set to the value of \code{plt.lat.axes}.
#' @param plt.lon.axes logical; shall longitude axes be plotted?
#' @param plt.lon.labels logical; shall longitude labels be plotted? Per default
#'   set to the value of \code{plt.lon.axes}.
#' @param rotate.long.labels logical; controls whether the longitude axis labels
#'   are rotated according to their value and the setting of \code{rotate} (the
#'   default) or not.
#' @param lat.ax.labs.pos longitudinal positions of the latitude axis
#'   labels. Per default, the labels are placed at 0 E (180 W) for north (south)
#'   polar plots or at the mean longitude of a segment; use this parameter to
#'   override the default setting.
#' @param ax.labs.size size of latitude and longitude axis labels.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of \code{"on"} (the default) means yes, and a setting of
#'   \code{"off"} means no. For details, please see
#'   \code{\link[ggplot2]{coord_cartesian}}.
#' @param data.layer optional ggplot2 layer of data onto which the polar map
#'   shall be plotted. Defaults to \code{NULL} which only plots the map.
#' @import ggplot2 maptools rgeos
#' @importFrom raster crop extent
#' @author Andrew Dolman <andrew.dolman@awi.de> with some modifications by
#'   Thomas Münch.
#' @source Adapted from Mikey Harper's plot; see
#'   https://stackoverflow.com/a/49084793.
#' @examples
#' library(ggplot2)
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4)
#' ggpolar(pole = "S", max.lat = -60, min.lat = -90)
#'
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'                max.lon = 0, min.lon = -80,
#'                longitude.spacing = 15, n.lat.labels = 5) +
#'   geom_point(aes(x = -35, y = 75, colour = "sd")) +
#'   geom_point(aes(x = -35, y = 70, colour = "sf"))
#'
#'
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'                max.lon = 0, min.lon = -80,
#'                longitude.spacing = 30,
#'                rotate = TRUE)
#'
#' ggpolar(pole = "S", max.lat = -55, min.lat = -90,
#'         max.lon = 80, min.lon = -20,
#'         longitude.spacing = 30,
#'         rotate = TRUE)
#'
#'
#' ggpolar(pole = "N", max.lat = 90, min.lat = 55,
#'                max.lon = 170, min.lon = -180,
#'                longitude.spacing = 33)
#'
#' # set country.outline.colour the same as land.fill.colour to hide political
#' # boundaries
#' ggpolar(pole = "N", max.lat = 90, min.lat = 65, n.lat.labels = 4,
#'  country.outline.colour = "Grey")
#'
#' \dontrun{
#' ggpolar(pole = "W", max.lat = -55, min.lat = -90)
#' ggpolar(pole = "S", max.lat = 90, min.lat = 55)
#' ggpolar(pole = "S", max.lat = -90, min.lat = -55)
#' }
#' @export
ggpolar <- function(pole = c("N", "S"),
                    max.lat, min.lat,
                    max.lon = 180, min.lon = -180,
                    longitude.spacing = 60,
                    land.fill.colour = "Grey",
                    country.outline.colour = "Black",
                    n.lat.labels = 4, nearest.x.degrees = 5,
                    f.long.label.ticks = 20, f.long.label.pos = 7,
                    lat.ax.vals = NULL, long.ax.vals = NULL,
                    rotate = FALSE, size.outer = 1,
                    plt.lat.axes = TRUE, plt.lat.labels = plt.lat.axes,
                    plt.lon.axes = TRUE, plt.lon.labels = plt.lon.axes,
                    rotate.long.labels = TRUE,
                    lat.ax.labs.pos = NULL, ax.labs.size = 4, clip = "on",
                    data.layer = NULL) {

  # force to repair invalid geometries
  rgeos::set_RGEOS_CheckValidity(2L)

  pole <- match.arg(pole, choices = c("N", "S"))

  if (pole == "N" & max.lat < 0) stop("If pole == N, max.lat should be positive")
  if (pole == "S" & min.lat > 0) stop("If pole == S, min.lat should be negative")

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
                        "°", ifelse(lat.ax.vals < 0, " S", " N"))

  # Define the x axes required
  if (!length(long.ax.vals)) {
    long.ax.vals <- seq(min.lon, max.lon, by = longitude.spacing)
  }

  if (!is.segment) {
    long.ax.vals <- long.ax.vals[long.ax.vals != -180]
  }

  long.ax.labs <- paste0(abs(long.ax.vals), "°",
                         ifelse(long.ax.vals <= 0, " W", " E"))
  long.ax.labs[long.ax.vals == 0] <- "0°"
  long.ax.labs[long.ax.vals == 180] <- "180° W"

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
  if (is.segment | pole == "N") {

    utils::data("wrld_simpl", package = "maptools")
    map.outline <- raster::crop(wrld_simpl,
                                raster::extent(min.lon, max.lon,
                                               min.lat, max.lat),
                                snap = "in")
  } else {

    # Use different map source for south polar full longitude plot
    # to circumvent buggy line at 180 deg W
    map.outline <- map_data("world", "Antarctica")
    i <- which(map.outline$lat >= min.lat & map.outline$lat <= max.lat)
    map.outline <- map.outline[i, ]
  }

  if (!length(data.layer)) {
    p <- ggplot()
  } else {
    p <- data.layer
  }

  p <- p +

    # Plot map outline and project to polar coordinates
    geom_polygon(data = map.outline, aes(x = long, y = lat, group = group),
                 fill = land.fill.colour, colour = country.outline.colour) +

    coord_map("ortho",
              orientation = c(ifelse(pole == "N", 90, -90), rotate.to, 0),
              xlim = c(min.lon, max.lon), clip = clip
              ) +

    # Remove axes and labels
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +

    # Outer latitude axis
    geom_line(aes(y = outer.lat.ax.val, x = min.lon : max.lon),
              size = size.outer, colour = "black")

    # Inner latitude axis
    if (!is.na(inner.lat.ax.val)) {
      p <- p + geom_line(aes(y = inner.lat.ax.val, x = min.lon : max.lon),
                         size = size.outer, colour = "black")
    }

    # Change theme to remove panel backgound
    p <- p + theme(panel.background = element_blank())

  # Add axes and labels

  # Lat axis lines
  if (plt.lat.axes) {
    p <- p + geom_line(data = lat.lines, aes(y = lat, x = long, group = lat),
                       size = 0.25, linetype = "dashed", colour = "black")
  }
  # Lat axis labels
  if (plt.lat.labels) {

    if (!length(lat.ax.labs.pos)) {
      lat.ax.labs.pos <- mean.lon
    }

    p <- p + geom_label(aes(x = lat.ax.labs.pos,
                            y = lat.ax.vals,
                            label = lat.ax.labs),
                        size = ax.labs.size,
                        alpha = 0.75, label.size = 0)
  }

  # Longitude axis lines
  if (plt.lon.axes) {

    p <- p + geom_segment(aes(y = long.line.strt, yend = long.line.end,
                              x = long.ax.vals, xend = long.ax.vals),
                          linetype = "dashed", colour = "black", size = 0.25)
  }
  # Longitude axis labels
  if (plt.lon.labels) {

    p <- p + geom_text(aes(x = long.ax.vals,
                           y = long.lab.pos.2,
                           label = long.ax.labs,
                           angle = long.ax.lab.rotation),
                       size = ax.labs.size)
  }

  # If segment add lines to edge
  if (is.segment) {
    p <- p + geom_segment(aes(y = long.line.strt, yend = long.line.end,
                              x = c(min.lon, max.lon), xend = c(min.lon, max.lon)),
                          colour = "black", size = size.outer)
  }

  p
}
