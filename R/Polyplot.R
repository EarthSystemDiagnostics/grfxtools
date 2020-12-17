#' Draw error shading
#'
#' A wrapper function for the \code{polygon} function to draw error shadings
#' (or confidence intervals) on a line plot.
#'
#' @param x numeric vector of x values of the error band.
#' @param y1 numeric vector for the upper bound of the error band; must be of
#' the same length as \code{x}.
#' @param y2 numeric vector for the lower bound of the error band; must be of
#' the same length as \code{x}.
#' @param col colour of the error band.
#' @param alpha opacity factor for \code{col} within [0,1].
#' @param ... additional parameters which are passed to \code{polygon}.
#' @seealso \code{\link[graphics]{polygon}}
#' @author Thomas Münch
#' @examples
#' x <- 1 : 10
#' plot(x, type = "n", xlab = "x", ylab = "y")
#' Polyplot(x, y1 = x + 2, y2 = x - 2)
#' lines(x, lwd = 2)
#' @export
Polyplot <- function(x, y1, y2, col = "black", alpha = 0.2, ...) {

  inp <- list(x, y1, y2)
  if (stats::var(sapply(inp, length)) != 0)
    stop("All input vectors must be of the same length.")
  if (any(sapply(inp, function(x){any(is.na(x))})))
    warning("Polyplot: Missing values as input.", call. = FALSE)

  col <- grDevices::adjustcolor(col = col, alpha = alpha)

  graphics::polygon(c(x, rev(x)), c(y1, rev(y2)),
                    col = col, border = NA, ...)

}
