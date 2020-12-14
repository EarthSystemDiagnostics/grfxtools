% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpolar.R
\name{ggpolar}
\alias{ggpolar}
\title{Nicely formatted polar ggplot}
\source{
Adapted from Mikey Harper's plot; see
  https://stackoverflow.com/a/49084793.
}
\usage{
ggpolar(
  pole = c("N", "S"),
  max.lat,
  min.lat,
  max.lon = 180,
  min.lon = -180,
  longitude.spacing = 60,
  land.fill.colour = "Grey",
  country.outline.colour = "Black",
  n.lat.labels = 4,
  nearest.x.degrees = 5,
  f.long.label.ticks = 20,
  f.long.label.pos = 7,
  lat.ax.vals = NULL,
  long.ax.vals = NULL,
  rotate = FALSE,
  size.outer = 1,
  plt.lat.axes = TRUE,
  plt.lat.labels = plt.lat.axes,
  plt.lon.axes = TRUE,
  plt.lon.labels = plt.lon.axes,
  rotate.long.labels = TRUE,
  lat.ax.labs.pos = NULL,
  ax.labs.size = 4,
  clip = "on",
  data.layer = NULL
)
}
\arguments{
\item{pole}{character; which pole: "N" or "S"?}

\item{max.lat}{maximum latitude.}

\item{min.lat}{minimum latitude.}

\item{max.lon}{maximum longitude.}

\item{min.lon}{minimum longitude.}

\item{longitude.spacing}{interval between longitude axis tick marks.}

\item{land.fill.colour}{colour to shade the land.}

\item{country.outline.colour}{colour for political boundaries (default
"Black"); set to the same as \code{land.fill.colour} to hide them.}

\item{n.lat.labels}{approximate number of latitude tickmarks.}

\item{nearest.x.degrees}{round latitude tickmarks to how many degrees?}

\item{f.long.label.ticks}{fraction of the plotted latitude axis range by
which the longitude ticksmarks extend behind the outer latitude axis,
i.e. the minimum (maximum) latitude for north (south) polar plots.}

\item{f.long.label.pos}{fraction of the plotted latitude axis range by
which the longitude labels are offset from the outer latitude axis.}

\item{lat.ax.vals}{manually set the latitude axis values where to plot
latitude labels and lines. This overrides the automatic setting by
\code{n.lat.labels} and \code{x.nearest.degress}, while the default
\code{NULL} means to use the automatic setting.}

\item{long.ax.vals}{manually set the longitude axis values where to plot
longitude labels and lines. This overrides the automatic setting by
\code{longitude.spacing}, while the default \code{NULL} means to use the
automatic setting.}

\item{rotate}{logical; if plotting a segment of < 360 degrees longitude,
rotate the plot so that north is up (or south is down) as seen from the
mean longitude of the segment.}

\item{size.outer}{size of the outer (and potential inner) longitude circle
and, if plotting a segment, of the outer latitude lines.}

\item{plt.lat.axes}{logical; shall latitude axes be plotted?}

\item{plt.lat.labels}{logical; shall latitude labels be plotted? Per default
set to the value of \code{plt.lat.axes}.}

\item{plt.lon.axes}{logical; shall longitude axes be plotted?}

\item{plt.lon.labels}{logical; shall longitude labels be plotted? Per default
set to the value of \code{plt.lon.axes}.}

\item{rotate.long.labels}{logical; controls whether the longitude axis labels
are rotated according to their value and the setting of \code{rotate} (the
default) or not.}

\item{lat.ax.labs.pos}{longitudinal positions of the latitude axis
labels. Per default, the labels are placed at 0 E (180 W) for north (south)
polar plots or at the mean longitude of a segment; use this parameter to
override the default setting.}

\item{ax.labs.size}{size of latitude and longitude axis labels.}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of
\code{"off"} means no. For details, please see
\code{\link[ggplot2]{coord_cartesian}}.}

\item{data.layer}{optional ggplot2 layer of data onto which the polar map
shall be plotted. Defaults to \code{NULL} which only plots the map.}
}
\description{
Produce nicely formatted maps of the Arctic or Antarctic in polar
projection. Also works for longitudinal segments, e.g. just Greenland.
}
\examples{
library(ggplot2)
ggpolar(pole = "N", max.lat = 90, min.lat = 55, n.lat.labels = 4)
ggpolar(pole = "S", max.lat = -60, min.lat = -90)

ggpolar(pole = "N", max.lat = 90, min.lat = 55,
               max.lon = 0, min.lon = -80,
               longitude.spacing = 15, n.lat.labels = 5) +
  geom_point(aes(x = -35, y = 75, colour = "sd")) +
  geom_point(aes(x = -35, y = 70, colour = "sf"))


ggpolar(pole = "N", max.lat = 90, min.lat = 55,
               max.lon = 0, min.lon = -80,
               longitude.spacing = 30,
               rotate = TRUE)

ggpolar(pole = "S", max.lat = -55, min.lat = -90,
        max.lon = 80, min.lon = -20,
        longitude.spacing = 30,
        rotate = TRUE)


ggpolar(pole = "N", max.lat = 90, min.lat = 55,
               max.lon = 170, min.lon = -180,
               longitude.spacing = 33)

# set country.outline.colour the same as land.fill.colour to hide political
# boundaries
ggpolar(pole = "N", max.lat = 90, min.lat = 65, n.lat.labels = 4,
 country.outline.colour = "Grey")

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