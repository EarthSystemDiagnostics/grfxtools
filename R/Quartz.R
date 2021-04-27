#' Open quartz device
#'
#' Wrapper to open a quartz device with default 4:3 dimensions for on-screen
#' plotting or saving to a file, including the setting of default graphical
#' parameters which are deliberately different from base R's setting to produce
#' visually more appealing plots.
#'
#' @param file path to a file for storing a hardcopy of the plot including a
#'   supported file extension to set the \code{type} of output (e.g. ".pdf" or
#'   ".png"); i.e. the function extracts the extension from \code{file} and
#'   uses it as the \code{type} argument for the call to
#'   \code{\link[grDevices]{quartz}}. Defaults to \code{NULL} for on-screen
#'   plotting.
#' @param type the type of output to use. Defaults to \code{"native"} for
#'   on-screen plotting. If \code{file} is not \code{NULL}, \code{type} is
#'   determined from the file name extension.
#' @param height the height of the plotting area in inches.  Default ‘6’.
#' @param width the width of the plotting area in inches.  Default ‘8’.
#' @inheritParams Par
#' @param ... further graphical parameter settings passed on to
#'   \code{\link{Par}}.
#' @inheritParams grDevices::quartz
#' @param dpi  resolution of the output. The default (‘NA_real_’) for an
#'   on-screen display defaults to the resolution of the main screen, and to 72
#'   dpi otherwise.
#'
#' @return invisibly, the original graphical parameter settings prior to the
#'   changes from a call to \code{Quartz} to enable restoring the default
#'   settings.
#' @seealso \code{\link[grDevices]{quartz}}; \code{\link{Par}}
#' @author Thomas Münch
#' @examples
#'
#' # Create an empty on-screen quartz device
#' Quartz()
#'
#' # Store empty plot in pdf format in local directory
#' \dontrun{
#' Quartz(file = file.path(getwd(), "test-quartz.pdf"))
#' dev.off()
#' }
#' @export
Quartz <- function(file = NULL, type = "native", height = 6, width = 8,
                   mar = c(5, 5, 0.5, 0.5), las = 1,
                   cex.axis = 1.25, cex.lab = 1.5, cex.main = 1.5, ...,
                   pointsize = 12, family = "Arial", antialias = TRUE,
                   bg = "transparent", canvas = "white", dpi = NA_real_) {

  # Determine file type from file extension
  if (!is.null(file)) {

    type <- tools::file_ext(file)
    if (nchar(type) == 0)
      stop("No file extension found for setting 'type'.")
    
  }

  # Open device
  quartz(width = width, height = height, file = file, type = type,
         pointsize = pointsize, family = family, antialias = antialias,
         bg = bg, canvas = canvas, dpi = dpi)
  op <- Par(mar = mar, las = las, cex.axis = cex.axis,
            cex.lab = cex.lab, cex.main = cex.main, ...)

  invisible(op)

}

#' Set new graphical parameters
#'
#' This function sets the graphical parameters which are specified as its
#' function arguments in a call to \code{\link[graphics]{par}}. The function's
#' default settings are set to produce visually more appealing plots compared to
#' base R's default settings. You can change the function's defaults to your
#' needs and/or set additional graphical parameters by passing them in \code{tag
#' = value} form.
#'
#' @param mar a numerical vector of the form ‘c(bottom, left, top, right)’ which
#'   gives the number of lines of margin to be specified on the four sides of
#'   the plot. The default is \code{c(5, 5, 0.5, 0.5)}.
#' @param las the style of axis labels; default is to always use horizontal axis
#'   labels (note that you then need to manually set \code{las = 0} for the y
#'   axis titles when using \code{mtext}).
#' @param cex.axis the magnification to be used for axis annotation relative to
#'   the current setting of \code{cex}; defaults to \code{1.25}.
#' @param cex.lab the magnification to be used for x and y labels relative to the
#'   current setting of \code{cex}; defaults to \code{1.5}.
#' @param cex.main the magnification to be used for main titles relative to the
#'   current setting of \code{cex}; defaults to \code{1.5}.
#' @param ... further graphical parameter settings passed on to
#'   \code{\link[graphics]{par}}.
#'
#' @return invisibly, the original graphical parameter settings prior to the
#'   changes from a call to \code{Par} to enable restoring the default
#'   settings; see the example.
#' @seealso \code{\link[graphics]{par}}
#' @author Thomas Münch
#' @examples
#' op <- Par()
#' plot(1 : 10, type = "l", main = "", xlab = "X title", ylab = "Y title")
#' par(op)
#' @export
Par <- function(mar = c(5, 5, 0.5, 0.5), las = 1,
                cex.axis = 1.25, cex.lab = 1.5, cex.main = 1.5, ...) {

  invisible(par(c(as.list(environment()), list(...))))

}
