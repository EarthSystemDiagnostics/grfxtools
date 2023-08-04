# Code adapted and simplified after Hmisc::minor.tick:
# GPL (>=2)
# Copyright (C) 2001 Frank E Harrel Jr

# Changes to original code by Thomas Muench in 2017:
# - minor ticks do not overplot exisiting major ticks, nor extend beyond them
# - direct setting of minor tick colours via function parameter
# - minor ticks can be drawn on top or right-hand side axes
# Further changes by Thomas Muench on 2023/08/01:
# - no extra axis line is drawn along with the minor ticks
# - optional extension of minor ticks beyond first/last major tick
# - further graphical parameters can be passed on via \code{...}
# - lwd of minor ticks can be set in function call

#' Draw aesthetic minor ticks
#'
#' Adds minor tick marks to an existing plot.
#'
#' This is a modified version of the function \code{minor.tick} from the
#' package \code{Hmisc}. Main differences are i) that minor ticks are only
#' plotted between the positions of the existing major ticks, not additionally
#' at the major ticks, but extending the minor ticks beyond the first and last
#' major tick is optional using the \code{extend} parameter, ii) allowing the 
#' direct setting of the colour and the line width of the minor ticks, and iii)
#' that minor ticks can be also drawn on axes at the top or the right-hand side
#' of the plot. The latter, however, makes it necessary to call \code{MinorTick}
#' twice if you want to plott minor ticks on both the x and y axes on the same
#' plot.
#'
#' @param n number of intervals in which to divide the area between major tick
#'   marks on the specified axis. Values <= 1 suppress minor tick marks.
#' @param side an integer specifying on which axis the minor ticks are to be
#' drawn on. The axis is denoted as follows: 1=below, 2=left, 3=above and
#'   4=right.
#' @param tick.ratio ratio of lengths of minor tick marks to major tick
#'   marks. The length of major tick marks is retrieved from ‘par("tcl")’.
#' @param col color of the minor ticks.
#' @param lwd line width for the minor ticks; defaults to 1.
#' @param extend an integer specifying the number of minor ticks to be plotted
#'   before the first and after the last major tick mark; if larger than
#'   \code{n}, it is automatically reduced to \code{n-1}. Default is to plot no
#'   extended minor ticks. Note that since minor ticks are drawn without an
#'   extra axis line, extended minor ticks are visually only useful if you use
#'   the plot bounding \code{box()} or another means of drawing a (longer) axis
#'   line.
#' @param ... further graphical parameters passed on to \code{axis} for
#'   plotting the minor ticks, see examples.
#' @author Frank Harrell, Earl Bellinger, Viktor Horvath, Thomas Münch
#' @source The original version of \code{minor.tick} is available in the
#'   \code{Hmisc} package from CRAN:
#'   <https://cran.r-project.org/web/packages/Hmisc/>
#' @examples
#'
#' plot(1 : 10)
#' MinorTick(n = 1) # <- no minor ticks
#' MinorTick()
#' MinorTick(side = 2, lwd = 3)
#' MinorTick(n = 4, side = 3)
#' MinorTick(n = 6, side = 4, col = 2)
#'
#' # plot extended minor ticks
#' x <- 2 : 9
#' plot(x = x, y = x, axes = FALSE, xlim = c(1, 10))
#' par(xaxp = c(2, 9, 7))
#' axis(1)
#' axis(2)
#' 
#' MinorTick(n = 3, extend = 2)
#' # <- looks unpleasant without a bounding box
#' box() # better
#'
#' # optional parameters that may need to be passed on include, e.g.,
#' # the line parameter:
#' plot(1 : 10, axes = FALSE, xlab = "")
#'
#' axis(2)
#' axis(1, line = 2)
#'
#' MinorTick() # <- incorrect positioning
#' MinorTick(line = 2) # here we go
#'
#' # you need to be careful when manually setting major tick mark locations
#' \dontrun{
#'   plot(1 : 10, xlim = c(0, 10), axes = FALSE)
#'   axis(1, at = seq(0.5, 9.5, 1))
#'   MinorTick(col = "red")
#'   # <- this will position the minor tick marks incorrectly, since
#'   # \code{MinorTick} extracts the major tick mark positions from
#'   # \code{xaxp} and \code{yaxp}, which are not updated by the manual setting
#'   # in \code{axis}
#' }
#' # this instead works:
#' plot(1 : 10, xlim = c(0, 10), axes = FALSE)
#' par(xaxp = c(0.5, 9.5, 9))
#' axis(1)
#' MinorTick(col = "red")
#' 
#' @export
MinorTick <- function(n = 2, side = 1, tick.ratio = 0.5, col = "black",
                      lwd = 1, extend = 0, ...) {

  ax <- function(w, n, tick.ratio, side, col, lwd, extend, ...) {
    tick.pos <- if (w == "x") 
                  graphics::par("xaxp")
                else graphics::par("yaxp")

    distance.between.major <- (tick.pos[2] - tick.pos[1]) / tick.pos[3]
    low.major <- low.minor <- tick.pos[1]
    hi.major <- hi.minor <- tick.pos[2]
    major.ticks <- seq(low.minor, hi.minor, by = distance.between.major)
    minor.ticks <- seq(low.minor, hi.minor, by = distance.between.major / n)
    minor.ticks <-
      minor.ticks[-which(!is.na(match(minor.ticks, major.ticks)))]

    if (extend > 0) {
      if (extend >= n) {
        extend <- n - 1
      }
      extension <- (1 : extend) * (distance.between.major / n)
      minor.ticks <- c(low.major - rev(extension), minor.ticks,
                       hi.major + extension)
    }

    graphics::axis(side, minor.ticks, lwd = 0, lwd.ticks = lwd, labels = FALSE,
                   tcl = graphics::par("tcl") * tick.ratio, col = col, ...)
  }
  if (n > 1) {
    if (side == 1 || side == 3) {
      ax("x", n = n, tick.ratio = tick.ratio, side = side,
         col = col, lwd = lwd, extend = extend, ...)
    } else {
      ax("y", n = n, tick.ratio = tick.ratio, side = side,
         col = col, lwd = lwd, extend = extend, ...)
    }
  }
  invisible()
}
