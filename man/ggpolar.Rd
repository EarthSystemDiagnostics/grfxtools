% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpolar.R
\name{ggpolar}
\alias{ggpolar}
\title{Nicely formatted polar ggplot}
\source{
Code adapted from Mikey Harper's plot; see
  https://stackoverflow.com/a/49084793.

  Coastlines and political boundaries for complete north polar plots, and for
  north and south polar segment plots, have been obtained from the
  rnaturalearth package:
  https://cran.r-project.org/web/packages/rnaturalearth/index.html

  Complete south polar plots use ggplot2 data.
}
\usage{
ggpolar(
  pole = c("N", "S"),
  data.layer = NULL,
  max.lat,
  min.lat,
  max.lon = 180,
  min.lon = -180,
  longitude.spacing = 60,
  n.lat.labels = 4,
  plot.political.boundaries = FALSE,
  land.fill.colour = "Grey",
  land.outline.colour = "Black",
  rotate = FALSE,
  nearest.x.degrees = 5,
  lat.ax.vals = NULL,
  long.ax.vals = NULL,
  plt.lat.axes = TRUE,
  plt.lat.labels = plt.lat.axes,
  plt.lon.axes = TRUE,
  plt.lon.labels = plt.lon.axes,
  f.long.label.ticks = 20,
  f.long.label.pos = 7,
  rotate.long.labels = TRUE,
  lat.ax.labs.pos = NULL,
  ax.labs.size = 4,
  size = 0.5,
  size.axes = 0.25,
  size.outer = 1,
  clip = "on"
)
}
\arguments{
\item{pole}{character; which pole: north ("N") or south ("S")? Note that
full-longitude south polar plots only include the Antarctic main land,
irrespective of the latitude setting.}

\item{data.layer}{optional ggplot2 layer of data onto which the polar map
shall be plotted. Defaults to \code{NULL} which only plots the map.}

\item{max.lat}{maximum latitude in degree.}

\item{min.lat}{minimum latitude in degree.}

\item{max.lon}{maximum longitude in degree.}

\item{min.lon}{minimum longitude in degree.}

\item{longitude.spacing}{interval between longitude axis tick marks.}

\item{n.lat.labels}{approximate number of latitude tick marks.}

\item{plot.political.boundaries}{logical; should political (country)
boundaries be plotted? Defaults to FALSE.}

\item{land.fill.colour}{colour to shade the land (default "Grey").}

\item{land.outline.colour}{colour for the land surface outlines (default
"Black"); set to the same colour as \code{land.fill.colour} to hide
them. This parameter also controls the colours of the political boundaries,
if they are switched on.}

\item{rotate}{logical; if plotting a segment of < 360 degrees longitude,
rotate the plot so that north is up (or south is down) as seen from the
mean longitude of the segment.}

\item{nearest.x.degrees}{round latitude tickmarks to how many degrees?}

\item{lat.ax.vals}{manually set the latitude axis values where to plot
latitude labels and lines. This overrides the automatic setting by
\code{n.lat.labels} and \code{x.nearest.degress}, while the default
\code{NULL} means to use the automatic setting.}

\item{long.ax.vals}{manually set the longitude axis values where to plot
longitude labels and lines. This overrides the automatic setting by
\code{longitude.spacing}, while the default \code{NULL} means to use the
automatic setting.}

\item{plt.lat.axes}{logical; shall latitude axes be plotted?}

\item{plt.lat.labels}{logical; shall latitude labels be plotted? Per default
set to the value of \code{plt.lat.axes}.}

\item{plt.lon.axes}{logical; shall longitude axes be plotted?}

\item{plt.lon.labels}{logical; shall longitude labels be plotted? Per default
set to the value of \code{plt.lon.axes}.}

\item{f.long.label.ticks}{fraction of the plotted latitude axis range by
which the longitude ticksmarks extend behind the outer latitude axis,
i.e. the minimum (maximum) latitude for north (south) polar plots.}

\item{f.long.label.pos}{fraction of the plotted latitude axis range by
which the longitude labels are offset from the outer latitude axis.}

\item{rotate.long.labels}{logical; controls whether the longitude axis labels
are rotated according to their value and the setting of \code{rotate} (the
default) or not.}

\item{lat.ax.labs.pos}{longitudinal positions of the latitude axis
labels. Per default, the labels are placed at 0 E (180 W) for north (south)
polar plots or at the mean longitude of a segment; use this parameter to
override the default setting.}

\item{ax.labs.size}{size in mm of latitude and longitude axis labels.}

\item{size}{size in mm of the continental and land outlines.}

\item{size.axes}{size in mm of the latitude and longitude axes.}

\item{size.outer}{size in mm of the outer (and potential inner) longitude
circle and, if plotting a segment, of the outer latitude lines.}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of
\code{"off"} means no. For details, please see
\code{\link[ggplot2]{coord_cartesian}}.}
}
\description{
Produce nicely formatted maps of the Arctic or Antarctic in polar
projection. Also works for longitudinal segments, e.g. just Greenland.
}
\examples{

# full north polar plot
ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4)

# with political boundaries
ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4,
        plot.political.boundaries = TRUE)

# hide land surface outlines
ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4,
        land.outline.colour = "Grey")

# full south polar plot
ggpolar(pole = "S", max.lat = -60, min.lat = -90)

# north polar segment plot with adjusted longitude and latitude labelling
ggpolar(pole = "N", max.lat = 90, min.lat = 55,
        max.lon = 0, min.lon = -80,
        longitude.spacing = 15, n.lat.labels = 5)

# rotate the segment
ggpolar(pole = "N", max.lat = 90, min.lat = 55,
        max.lon = 0, min.lon = -80,
        longitude.spacing = 15, n.lat.labels = 5,
        rotate = TRUE)

# south polar segment plot
ggpolar(pole = "S", max.lat = -55, min.lat = -90,
        max.lon = 80, min.lon = -20,
        longitude.spacing = 30, rotate = TRUE)

# it is also possible to plot a ring segment;
# additionally, there are various options to cuzstomize position and look of
# axis labels etc., with a few examples shown here
ggpolar(pole = "N", max.lat = 85, min.lat = 57.5,
  max.lon = -10, min.lon = -75, rotate = TRUE,
  lat.ax.vals = c(60, 70, 80),   # explicitly specify latitude labels
  long.ax.vals = -c(20, 40, 60), # explicitly specify longitude labels
  f.long.label.ticks = Inf,      # don't draw longitude lines beyond segment
  f.long.label.pos = 15,         # adjust label position from segment outline
  rotate.long.labels = FALSE,    # don't rotate longitude labels
  lat.ax.labs.pos = -75 - c(3.5, 5, 10), # move latitude labels to left side
  ax.labs.size = 4.75)                   # larger labels

# ---------------------------------------------------------------------------

# if you want to add points or a spatial field of data to a ggpolar plot, it
# can be convenient to create that plot separately and then provide it as an
# argument to ggpolar, so that it can be added to the polar plot internally:

library(ggplot2)

# create some dummy data
nx <- 360 / 5
ny <- 30 / 5

lon <- seq(0, 355, length.out = nx)
lat <- seq(-65, -90, length.out = ny)

df <- data.frame(lon = rep(lon, times = ny), lat = rep(lat, each = nx),
                 dat = runif(n = nx * ny, min = -1, max = 1))

# Colorbrewer2 colour scales for plotting the "correlation" data can be
# obtained directly with the respective grfxtools function
colour.scale <- ColorPal("RdBu", rev = TRUE)

# create ggplot for data
p <- ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = dat),
            data = df, colour = "transparent") +
  scale_fill_gradientn(colours = colour.scale,
                       limits = c(-1, 1),
                       name = "Correlation") +
  theme(legend.key.height = unit(0.75, units = "inches"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        text = element_text(size = 18))

# plot data projected on south polar map
ggpolar(pole = "S", data.layer = p,
        max.lat = -60, min.lat = -90, n.lat.labels = 3,
        longitude.spacing = 45,
        land.fill.colour = "transparent")

# ---------------------------------------------------------------------------

\dontrun{
ggpolar(pole = "W", max.lat = -55, min.lat = -90)
ggpolar(pole = "S", max.lat = 90, min.lat = 55)
ggpolar(pole = "S", max.lat = -90, min.lat = -55)
}
}
\author{
Andrew Dolman <andrew.dolman@awi.de> with some modifications by
  Thomas Münch.
}
