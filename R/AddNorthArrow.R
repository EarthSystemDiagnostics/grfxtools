# Code adapted from function addnortharrow in package prettymapr v0.2.4:
# GPL-2
# Copyright (C) 2022 Dewey Dunnington

# Changes to original code by Thomas Muench on 2023/08/03:
# - enable user setting of text size for "N"
# - rename text.col to col.text
# - enable user label position adjustment relative to north arrow
# - revised function documentation

#' Plot north arrow
#'
#' Plot a north arrow (pointing directly "up" and with label "N") positioned
#' based on current plot extents.
#'
#' @param pos where to align the north arrow. One of "bottomleft",
#'   "bottomright", "topleft", or "topright".
#' @param padin a vector of length 2 determining the distance in inches between
#'   the scalebar and the edge of the plottable area.
#' @param scale scale the north arrow to make it bigger or smaller.
#' @param lwd the line width outlining the north arrow.
#' @param border the line color outlining the north arrow.
#' @param cols a vector of length 2 determining the two colors to be drawn for
#'   the north arrow.
#' @param col.text color of the text label.
#' @param cex.text character expansion factor for the text label. This is an
#'   absolute measure, not scaled by \code{par("cex")} or by setting
#'   \code{par("mfrow")} or \code{par("mfcol")}.
#' @param vadj.text value in [0, 1] to specify the vertical adjustment of the
#'   text label relative to the north arrow, with 0 for top, 1 for bottom, and
#'   0.5 for centered position. On most devices values outside [0, 1] will also
#'   work.
#' @author Dewey Dunnington, Thomas Münch
#' @source Adapted from
#'   <https://CRAN.R-project.org/package=prettymapr>
#' @examples
#'
#' plot(1 : 5, 1 : 5, asp = 1)
#' AddNorthArrow()
#'
#' @export
AddNorthArrow <- function (pos = "topright", padin = c(0.15, 0.15), scale = 1,
                           lwd = 1, border = "black",
                           cols = c("white", "black"), col.text = "black",
                           cex.text = 1, vadj.text = 0.5)
{
  extents <- graphics::par("usr")
  arrow1.x <- c(-0.5, 0, 0)
  arrow1.y <- c(-0.5, 0.5, 0)
  arrow2.x <- -1 * arrow1.x
  arrow2.y <- arrow1.y
  htin <- 0.6 * scale
  wdin <- 0.6 * scale
  bottomin <- graphics::grconvertY(extents[3], from = "user", 
                                   to = "inches")
  leftin <- graphics::grconvertX(extents[1], from = "user", 
                                 to = "inches")
  topin <- graphics::grconvertY(extents[4], from = "user", 
                                to = "inches")
  rightin <- graphics::grconvertX(extents[2], from = "user", 
                                  to = "inches")
  textheight <- graphics::strheight("N", units = "user", cex = cex.text) ## <- tmuench, 2023/08/03
  if (pos == "bottomleft") {
    x <- graphics::grconvertX(leftin + padin[1], from = "inches", 
                              to = "user")
    y <- graphics::grconvertY(bottomin + padin[2], from = "inches", 
                              to = "user") + textheight/2
    adj <- c(0, 0)
  }
  else if (pos == "topleft") {
    x <- graphics::grconvertX(leftin + padin[1], from = "inches", 
                              to = "user")
    y <- graphics::grconvertY(topin - padin[2], from = "inches", 
                              to = "user")
    adj <- c(0, 1)
  }
  else if (pos == "topright") {
    x <- graphics::grconvertX(rightin - padin[1], from = "inches", 
                              to = "user")
    y <- graphics::grconvertY(topin - padin[2], from = "inches", 
                              to = "user")
    adj <- c(1, 1)
  }
  else if (pos == "bottomright") {
    x <- graphics::grconvertX(rightin - padin[1], from = "inches", 
                              to = "user")
    y <- graphics::grconvertY(bottomin + padin[2], from = "inches", 
                              to = "user") + textheight/2
    adj <- c(1, 0)
  }
  yin <- graphics::grconvertY(y, from = "user", to = "inches")
  xin <- graphics::grconvertX(x, from = "user", to = "inches")
  ht <- graphics::grconvertY(yin + htin, from = "inches", to = "user") - 
    y
  wd <- graphics::grconvertX(xin + wdin, from = "inches", to = "user") - 
    x
  graphics::polygon(x - adj[1] * wd + arrow1.x * wd + wd/2, 
                    y - adj[2] * ht + arrow1.y * ht + ht/2, border = border, 
                    lwd = lwd, col = cols[1])
  graphics::polygon(x - adj[1] * wd + arrow2.x * wd + wd/2, 
                    y - adj[2] * ht + arrow2.y * ht + ht/2, border = border, 
                    lwd = lwd, col = cols[2])
  graphics::text(x - adj[1] * wd + wd/2, y - adj[2] * ht + ht * 0, "N",
                 cex = cex.text, adj = c(0.5, vadj.text), col = col.text)
  ## <- tmuench, 2023/08/03
}
