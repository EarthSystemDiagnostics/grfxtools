#' Add session information to plot
#'
#' Add session information to an existing base plot: script name, date of plot
#' creation, etc., placed in the upper right corner of the plot.
#'
#' @param script character string with the name of the script used for creating
#'   the plot.
#' @param add.info character string with additional information to be added
#'   (optional).
#' @param incr numeric; fraction of the user coordinate range to use as
#'   incremental distance from the default position of the information text in
#'   the upper right corner of the plot region.
#' @param cex the magnification to be used for the information text relative to
#'   the current setting of \code{cex}; defaults to \code{0.75}.
#'
#' @author Thomas Münch
#' @examples
#' plot(1 : 10, type = "l")
#' PlotStamp(script = "interactive",
#'           add.info = "Illustrated usage of PlotStamp function")
#' @export
PlotStamp <- function(script, add.info = NULL, incr = 0.01, cex = 0.75) {

    if (is.null(add.info)) {
        info <- sprintf("Date: %s\nsrc: %s", Sys.time(), script)
    } else {
        info <- sprintf("Date: %s\nsrc: %s\nInfo:\n%s.",
                        Sys.time(), script, add.info)
    }

    usr <- par("usr")
    add <- incr * c(diff(range(usr[1 : 2])), diff(range(usr[3 : 4])))
    text(usr[1] + add[1], usr[4] - add[2], info, adj = c(0, 1),
         cex = cex * par("cex"))

}
