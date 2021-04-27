#' Add error bars
#'
#' Add error bars to an existing point plot.
#'
#' @param x x coordinates of the points for which error bars are to be added.
#' @param y y coordinates of the points for which error bars are to be added.
#' @param upper upper value of the error interval relative to \code{y}; i.e.,
#'   the upper end of the error interval extends to \code{y + upper}.
#' @param lower same as \code{upper} for the lower value of the error interval;
#'   default is to assume symmetric error intervals around \code{y}.
#' @param width the width of the error bar drawing as a fraction of the x user
#'   coordinate range (\code{diff(range(par("usr")[1 : 2]))}); defaults to five
#'   percent.
#' @param col the colour of the error bars; default "black".
#' @param lwd the line width of the error bars; default \code{1}.
#' @param lty the line type of the error bars; default are solid lines
#'   (\code{lty = 1}).
#'
#' @author Thomas Münch
#' @examples
#' x <- 1 : 10
#' plot(x, ylim = c(0, 12))
#' ErrorBars(x, y = x, upper = 2, lower = 1)
#' ErrorBars(x, y = x, upper = 0.5, col = "red", width = 0.025)
#' @export
ErrorBars <- function(x, y, upper, lower = upper, width = 0.05,
                      col = "black", lwd = 1, lty = 1) {

    if (length(x) != length(y)) stop("x and y lengths differ.")

    usr <- par("usr")
    w <- width * diff(range(usr[1 : 2]))
    width <- w / 2

    segments(x, y - lower, x, y + upper, col = col,
             lwd = lwd, lty = lty, lend = 1)
    segments(x - width, y - lower, x + width, y - lower, col = col,
             lwd = lwd, lty = lty, lend = 1)
    segments(x - width, y + upper, x + width, y + upper, col = col,
             lwd = lwd, lty = lty, lend = 1)

}
