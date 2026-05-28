#' MapPlotly
#'
#' @description Create interactive maps (plotly htmlwidgets) with many different projections. 
#' @param proj name of the projection to use
#' @param resolution resolution of the base map, options are 110 or 50 km per mm. Defaults to 110 km/mm
#' @param focal.lat centre on this latitude
#' @param focal.lon centre on this longitude
#' @param lon.for.latlabels put a line of latitude labels along this longitude (can use multiple)
#' @param lat.for.lonlabels put a line of longitude labels along this latitude (can use multiple)
#' @param lon.lines plot these longitude lines as a graticule
#' @param lat.lines plot these latitude lines as a graticule
#' @param lat.range restrict the plot to this latitude range e.g. c(-30, 30) 
#' @param lon.range restrict the plot to this longitude range e.g. c(-30, 90)
#' @param show.longrid,show.latgrid show fine gridlines 
#' @param land.color fill colour for land
#' @param ocean.color fill colour of ocean
#' @param colors colour palette passed to \code{plotly::plot_geo}
#'
#' @import plotly
#' @importFrom tidyr crossing
#' @importFrom dplyr mutate filter group_by
#' @return a plotly widget
#' @export
#' @examples
#' \dontrun{
#' # Mollenweide
#' p <- MapPlotly(proj = "moll")
#' p
#' 
#' # Mollenweide with North Pole focus
#' p <- MapPlotly(proj = "moll", focal.lat = 90)
#' p
#' 
#' p <- MapPlotly(proj = "azimuthal equal area", focal.lat = 90)
#' p
#' 
#' # Not so great for whole globe, but limit lat
#' 
#' p <- MapPlotly(proj = "azimuthal equal area", focal.lat = 90, lat.range  = c(0, 90))
#' p
#' 
#' p <- MapPlotly(proj = "azimuthal equal area", focal.lat = 0, lon.range = c(-60, 60))
#' p 
#' 
#' # basemap a little low res for small areas
#' p <- MapPlotly(
#'  proj = "azimuthal equal area", focal.lat = 53, lon.range = c(-35, 35), lat.range = c(30, 60) )
#' p
#' 
#' # switch off labels for some things
#' p <- MapPlotly(proj = "moll", focal.lat = 0, focal.lon = 0, 
#'           lon.for.latlabels = NA, lat.for.lonlabels = NA,
#'           lon.lines = NA, lat.lines = NA,
#'           show.latgrid = FALSE, show.longrid = FALSE)
#' p
#' 
#' 
#' # Add datapoints (see plotly for more things like this)
#'
#' dat <- data.frame(lat = c(15, 45), lon = c(22, -90),
#'               type = c("Pollen", "Tree ring"))
#'
#' plotly::add_markers(p, data = dat, showlegend = TRUE,
#'               color = I("blue"))
#'
#'
#'
#' p <- MapPlotly(proj = "ortho", focal.lat = 90, focal.lon = 135,
#'           # lon.lines = seq(-80, -10, 10),
#'           # lon.for.latlabels = -55, lat.for.lonlabels = 65,
#'           lat.range = c(0, 90))
#' p <- plotly::add_markers(p, data = dat, showlegend = TRUE,
#'               color = I("blue"))
#' p
#' 
#' p <- MapPlotly(proj = "ortho", focal.lat = 0, focal.lon = 0,
#'                lon.range = c(0, 360),
#'                lat.range = c(-90, 90))  
#' 
#' p
#' 
#' p <- MapPlotly(proj = "eckert4",
#'                focal.lat = 0, focal.lon = -90          )
#' p
#' 
#' p <- MapPlotly(proj = "eckert4",
#'                focal.lat = 0, focal.lon = -90, lat.range = c(-40, 40), lon.range = c(45, 170))
#' p
#' 
#' 
#' p <- MapPlotly(proj = "eckert4",
#'                focal.lat = 90, focal.lon = 135)
#' p
#' }
MapPlotly <- function(proj = c('equirectangular',
                               'mercator',
                               'orthographic',
                               'natural earth',
                               'kavrayskiy7',
                               'miller',
                               'robinson',
                               'eckert4',
                               'azimuthal equal area',
                               'azimuthal equidistant',
                               'conic equal area',
                               'conic conformal',
                               'conic equidistant',
                               'gnomonic',
                               'stereographic',
                               'mollweide',
                               'hammer',
                               'transverse mercator',
                               # 'albers usa',
                               'winkel tripel',
                               'aitoff',
                               'sinusoidal'),
                      resolution = 110,
                      focal.lat = 0, focal.lon = 0,
                      lon.for.latlabels = -30,
                      lat.for.lonlabels = 15,
                      lon.lines = seq(-180, 180, 45),
                      lat.lines = seq(-90, 90, 30),
                      lat.range = c(-90, 90),
                      lon.range = c(0, 360),
                      show.longrid = TRUE,
                      show.latgrid = TRUE,
                      land.color = "#e5ecf6",
                      ocean.color = "white",
                      colors = "Set2"){
  
  proj <- match.arg(proj)
  
  df.lon <- tidyr::crossing(lon = lon.lines, lat = seq(-90, 90))
  df.lon <- dplyr::group_by(df.lon, .data$lon)

  df.lon.labs <- tidyr::crossing(lat = lat.for.lonlabels,
                                 lon = unique(df.lon$lon))
  df.lon.labs <- dplyr::mutate(df.lon.labs,
    lab = paste0(abs(.data$lon), ifelse(.data$lon >= 0, "\u00b0 E", "\u00b0 W")))
  df.lon.labs <- dplyr::filter(df.lon.labs, .data$lon != -180)

  df.lat <- tidyr::crossing(lat = lat.lines, lon = seq(0, 360, 1))
  df.lat <- dplyr::group_by(df.lat, .data$lat)

  df.lat.labs <- tidyr::crossing(lon = lon.for.latlabels,
                                 lat = unique(df.lat$lat))
  df.lat.labs <- dplyr::mutate(df.lat.labs,
    lab = paste0(abs(.data$lat), ifelse(.data$lat >= 0, "\u00b0 N", "\u00b0 S")))
  
  if (resolution %in% c(50, 110) == FALSE){
    stop("Resolution must be one of 110 or 50 [km/mm]")
  }
  
  g <- list(
    scope = 'world',
    resolution = resolution,
    projection = list(
      type = proj,
      rotation = list(lon=focal.lon, lat=focal.lat, roll=0
      )
    ),
    
    lonaxis = list(
      showgrid = show.longrid,
      gridwidth = 0.5,
      range = lon.range,
      dtick = 5
    ),
    
    lataxis = list(
      showgrid = show.latgrid,
      gridwidth = 0.5,
      range = (lat.range),
      dtick = 5
    ),
    
    center = list(lon=focal.lon, lat=focal.lat),
    showland = TRUE,
    landcolor = plotly::toRGB(land.color),
    showocean = TRUE,
    oceancolor = plotly::toRGB(ocean.color),
    color = plotly::toRGB("red")
  )
  
  
  df <- data.frame(lat = NA, lon = NA)
  
  
  p <- plotly::plot_geo(data = df, lat = ~lat, lon = ~lon, colors = colors) 
  p <- plotly::add_paths(p, data = df.lon, color = I("grey"),
                         line = list(dash = "dot", width = 1),
                         showlegend = F) 
  p <- plotly::add_paths(p, data = df.lat, color = I("grey"),
                         line = list(dash = "dot", width = 1),
                         showlegend = F) 
  p <-   plotly::add_text(p, x = ~ lon, y = ~lat, text = ~lab, data = df.lon.labs,
                          showlegend = F) 
  p <-   plotly::add_text(p, x = ~ lon, y = ~lat, text = ~lab, data = df.lat.labs,
                          showlegend = F) 
  p <-   plotly::layout(p, geo = g, showlegend  = T)
  
  return(p)
}


#' SetupPlotlySave
#'
#' @description Install the Python kaleido package needed to save plotly figures
#'   as static images via \code{SaveMapPlotly}. Only needs to be run once per
#'   machine. Requires the \pkg{reticulate} package.
#' @export
#' @examples
#' \dontrun{
#' SetupPlotlySave()
#' }
SetupPlotlySave <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install with: install.packages('reticulate')")
  }
  reticulate::install_miniconda(force = FALSE)
  reticulate::conda_install("r-reticulate", "python-kaleido")
  reticulate::conda_install("r-reticulate", "plotly", channel = "plotly")
  message("kaleido installed. Use SaveMapPlotly() to export static images.")
}


#' SaveMapPlotly
#'
#' @description Save a plotly map widget as a static image (PNG, SVG, PDF,
#'   WebP). Requires kaleido to be installed; run \code{\link{SetupPlotlySave}}
#'   once if this is your first time saving plotly figures.
#' @param p a plotly object, e.g. the output of \code{\link{MapPlotly}}
#' @param file output file path; the extension controls the format
#'   (\code{.png}, \code{.svg}, \code{.pdf}, \code{.webp})
#' @param width image width in pixels (default 1024)
#' @param height image height in pixels (default 768)
#' @param scale scale multiplier applied to \code{width} and \code{height};
#'   use 2 for high-DPI PNG output (default 2)
#' @export
#' @examples
#' \dontrun{
#' p <- MapPlotly(proj = "mollweide")
#' SaveMapPlotly(p, "map.png")
#' SaveMapPlotly(p, "map.svg", scale = 1)
#' }
SaveMapPlotly <- function(p, file, width = 1024, height = 768, scale = 2) {
  plotly::save_image(p, file = file, width = width, height = height, scale = scale)
}

