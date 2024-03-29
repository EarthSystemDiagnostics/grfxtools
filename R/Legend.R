# Code adapted from base graphics::legend as of R version 4.3:
# GPL-2 | GPL-3
# Copyright (C) 2023 R Core Team

# Changes to original code by Thomas Muench on 2023/08/01:
# - introduced function parameters `end.pch` and `pch.xoff` to enable adding
#   point characters to the end of legend lines

#' Add legends to plots
#' 
#' This modified version of the base R function \code{\link[graphics]{legend}}
#' modifies the behaviour when plotting a legend on a point plot connected by
#' lines: In this case, the base version adds the point character in the middle
#' of the legend line, whereas this version adds the point characters to both
#' ends of the legend line, when the parameter `end.pch` is set to
#' \code{TRUE}. The positioning of the end point charaters can be controlled by
#' \code{pch.xoff}.
#'
#' @param end.pch if \code{TRUE}, character symbols as specified by \code{pch}
#' are added to the end of the legend lines specified by \code{lty}. Defaults
#' to \code{FALSE} which reproduces the base behaviour.
#' @param pch.xoff horizontal adjustment of the point characters at the end
#' points of the legend lines to improve visual appearance; in units of
#' character widths (defaults to 0.2).
#' @inheritParams graphics::legend
#' @return the value returned by \code{\link[graphics]{legend}}.
#' @seealso \code{\link[graphics]{legend}}
#' @source \code{\link[graphics]{legend}}
#' @author R Core Team, Thomas Münch
#' @examples
#'
#' plot(x = 1 : 5, y = c(1, 5, 8, 3, 7), type = "b", pch = 19,
#'      xlab = "x", ylab = "y", ylim = c(0, 10))
#' legend("topleft", legend = "line with points from base `legend`", bty = "n",
#'        lty = 1, pch = 19)
#' Legend("topright", legend = "line with points from `Legend`", bty = "n",
#'        lty = 1, pch = 19, end.pch = TRUE, pch.xoff = 0.6, x.intersp = 1.5)
#' @export
Legend <- function (x, y = NULL, legend, fill = NULL, col = graphics::par("col"),
                    border = "black", lty, lwd, pch, angle = 45, density = NULL,
                    bty = "o", bg = graphics::par("bg"), box.lwd = graphics::par("lwd"),
                    box.lty = graphics::par("lty"), box.col = graphics::par("fg"), pt.bg = NA,
                    cex = 1, pt.cex = cex, pt.lwd = lwd, xjust = 0, yjust = 1,
                    x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
                    text.width = NULL, text.col = graphics::par("col"), text.font = NULL,
                    merge = do.lines && has.pch, trace = FALSE, plot = TRUE,
                    ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd,
                    title.col = text.col[1], title.adj = 0.5,
                    title.cex = cex[1], title.font = text.font[1], seg.len = 2,
                    end.pch = FALSE, pch.xoff = 0.2)
{
  if (missing(legend) && !missing(y) && (is.character(y) || 
                                         is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (!missing(xpd)) {
    op <- graphics::par("xpd")
    on.exit(graphics::par(xpd = op))
    graphics::par(xpd = xpd)
  }
  text.font <- if (is.null(text.font)) 
                 graphics::par("font")
               else text.font
  title <- grDevices::as.graphicsAnnot(title)
  if (length(title) > 1) 
    stop("invalid 'title'")
  legend <- grDevices::as.graphicsAnnot(legend)
  if (any(sapply(legend, is.language))) 
    legend <- as.expression(legend)
  n.leg <- length(legend)
  if (n.leg == 0) 
    stop("'legend' is of length 0")
  auto <- if (is.character(x)) 
            match.arg(x, c("bottomright", "bottom", "bottomleft", 
                           "left", "topleft", "top", "topright", "right", "center"))
          else NA
  if (is.na(auto)) {
    xy <- grDevices::xy.coords(x, y, setLab = FALSE)
    x <- xy$x
    y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2) 
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  reverse.xaxis <- graphics::par("xaxp")[1] > graphics::par("xaxp")[2]
  reverse.yaxis <- graphics::par("yaxp")[1] > graphics::par("yaxp")[2]
  xlog <- graphics::par("xlog")
  ylog <- graphics::par("ylog")
  cex <- rep(cex, length.out = n.leg)
  x.intersp <- rep(x.intersp, length.out = n.leg)
  seg.len <- rep(seg.len, length.out = n.leg)
  rect2 <- function(left, top, dx, dy, density = NULL, angle, 
                    ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    graphics::rect(left, top, r, b, angle = angle, density = density, 
         ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    graphics::segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if (xlog) 
      x <- 10^x
    if (ylog) 
      y <- 10^y
    graphics::points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog) 
      x <- 10^x
    if (ylog) 
      y <- 10^y
    graphics::text(x, y, ...)
  }
  colwise <- function(x, n, ncol, n.legpercol, fun, reverse = FALSE) {
    xmat <- matrix(c(rep(x, length.out = n), rep(0L, n.legpercol * 
                                                     ncol - n)), ncol = ncol)
    res <- apply(xmat, 2, fun)
    res[res == 0L] <- max(res)
    if (reverse) 
      -res
    else res
  }
  rowwise <- function(x, n, ncol, n.legpercol, fun, reverse = FALSE) {
    xmat <- matrix(c(rep(x, length.out = n), rep(0L, n.legpercol * 
                                                     ncol - n)), ncol = ncol)
    res <- apply(xmat, 1, fun)
    if (reverse) 
      -res
    else res
  }
  if (trace) {
    catn <- function(...) do.call(cat, c(lapply(list(...), 
                                                formatC), "\n"))
    fv <- function(...) paste(vapply(lapply(list(...), formatC), 
                                     paste, collapse = ",", ""), collapse = ", ")
  }
  n.legpercol <- if (horiz) {
                   if (ncol != 1) 
                     warning(gettextf("horizontal specification overrides: Number of columns := %d", 
                                      n.leg), domain = NA)
                   ncol <- n.leg
                   1
                 }
                 else ceiling(n.leg/ncol)
  Cex <- cex * graphics::par("cex")
  if (is.null(text.width)) 
    text.width <- max(abs(mapply(graphics::strwidth, legend, cex = cex, 
                                 font = text.font, MoreArgs = list(units = "user"))))
  else if ((length(text.width) > 1L && any(is.na(text.width))) || 
           (all(!is.na(text.width)) && (!is.numeric(text.width) || 
                                        any(text.width < 0)))) 
    stop("'text.width' must be numeric, >= 0, or a scalar NA")
  if (auto.text.width <- all(is.na(text.width))) {
    text.width <- abs(mapply(graphics::strwidth, legend, cex = cex, 
                             font = text.font, MoreArgs = list(units = "user")))
    ncol <- ceiling(n.leg/n.legpercol)
  }
  xyc <- graphics::xyinch(graphics::par("cin"), warn.log = FALSE)
  xc <- Cex * xyc[1L]
  yc <- Cex * xyc[2L]
  if (any(xc < 0)) 
    text.width <- -text.width
  xchar <- xc
  xextra <- 0
  y.intersp <- rep(y.intersp, length.out = n.legpercol)
  yextra <- rowwise(yc, n = n.leg, ncol = ncol, n.legpercol = n.legpercol, 
                    fun = function(x) max(abs(x)), reverse = reverse.yaxis) * 
    (y.intersp - 1)
  ymax <- sign(yc[1]) * max(abs(yc)) * max(1, mapply(graphics::strheight, 
                                                     legend, cex = cex, font = text.font, MoreArgs = list(units = "user"))/yc)
  ychar <- yextra + ymax
  ymaxtitle <- title.cex * graphics::par("cex") * xyc[2L] * max(1, graphics::strheight(title, 
                                                                   cex = title.cex, font = title.font, units = "user")/(title.cex * 
                                                                                                                        graphics::par("cex") * xyc[2L]))
  ychartitle <- yextra[1] + ymaxtitle
  if (trace) 
    catn("  xchar=", fv(xchar), "; (yextra, ychar)=", fv(yextra, 
                                                         ychar))
  if (mfill) {
    xbox <- xc * 0.8
    ybox <- yc * 0.5
    dx.fill <- max(xbox)
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
                                                          0))) || !missing(lwd)
  has.pch <- !missing(pch) && length(pch) > 0
  if (do.lines) {
    x.off <- if (merge) 
               -0.7
             else 0
  }
  else if (merge) 
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  if (has.pch) {
    if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L], 
                                                      type = "c") > 1) {
      if (length(pch) > 1) 
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type = "c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
    if (!is.character(pch)) 
      pch <- as.integer(pch)
  }
  if (is.na(auto)) {
    if (xlog) 
      x <- log10(x)
    if (ylog) 
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top <- y[2L]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust)) 
      xjust <- 0.5
    if (missing(yjust)) 
      yjust <- 0.5
  }
  else {
    yc <- rowwise(yc, n.leg, ncol, n.legpercol, fun = function(x) max(abs(x)), 
                  reverse = reverse.yaxis)
    h <- sum(ychar) + yc[length(yc)] + (!is.null(title)) * 
      ychartitle
    xch1 <- colwise(xchar, n.leg, ncol, n.legpercol, fun = function(x) max(abs(x)), 
                    reverse = reverse.xaxis)
    x.interspCol <- colwise(x.intersp, n.leg, ncol, n.legpercol, 
                            fun = max)
    seg.lenCol <- colwise(seg.len, n.leg, ncol, n.legpercol, 
                          fun = max)
    text.width <- colwise(text.width, n = if (auto.text.width) 
                                            n.leg
                                          else ncol, ncol, n.legpercol = if (auto.text.width) 
                                                                           n.legpercol
                                                                         else 1, fun = function(x) max(abs(x)), reverse = reverse.xaxis)
    w0 <- text.width + (x.interspCol + 1) * xch1
    if (mfill) 
      w0 <- w0 + dx.fill
    if (do.lines) 
      w0 <- w0 + (seg.lenCol + x.off) * xch1
    w <- sum(w0) + 0.5 * xch1[ncol]
    if (!is.null(title) && (abs(tw <- graphics::strwidth(title, units = "user", 
                                               cex = title.cex, font = title.font) + 0.5 * title.cex * 
                                      graphics::par("cex") * xyc[1L])) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- graphics::par("usr")
      inset <- rep_len(inset, 2)
      insetx <- inset[1L] * (usr[2L] - usr[1L])
      left <- switch(auto, bottomright = , topright = , 
                     right = usr[2L] - w - insetx, bottomleft = , 
                     left = , topleft = usr[1L] + insetx, bottom = , 
                     top = , center = (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L] * (usr[4L] - usr[3L])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                                                       h + insety, topleft = , top = , topright = usr[4L] - 
                                                                                         insety, left = , right = , center = (usr[3L] + 
                                                                                                                              usr[4L] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace) 
      catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  xt <- left + xc + xextra + rep(c(0, cumsum(w0))[1L:ncol], 
                                 each = n.legpercol, length.out = n.leg)
  topspace <- 0.5 * ymax + (!is.null(title)) * ychartitle
  yt <- top - topspace - cumsum((c(0, ychar)/2 + c(ychar, 0)/2)[1L:n.legpercol])
  yt <- rep(yt, length.out = n.leg)
  if (mfill) {
    if (plot) {
      if (!is.null(fill)) 
        fill <- rep_len(fill, n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
            col = fill, density = density, angle = angle, 
            border = border)
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines)) 
    col <- rep_len(col, n.leg)
  if (missing(lwd) || is.null(lwd)) 
    lwd <- graphics::par("lwd")
  if (do.lines) {
    if (missing(lty) || is.null(lty)) 
      lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) & 
      !is.na(lwd)
    if (trace) 
      catn("  segments2(", xt[ok.l] + x.off * xchar[ok.l], 
           ",", yt[ok.l], ", dx=", (seg.len * xchar)[ok.l], 
           ", dy=0, ...)")
    if (plot) 
      segments2(xt[ok.l] + x.off * xchar[ok.l], yt[ok.l], 
                dx = (seg.len * xchar)[ok.l], dy = 0, lty = lty[ok.l], 
                lwd = lwd[ok.l], col = col[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep_len(pch, n.leg)
    pt.bg <- rep_len(pt.bg, n.leg)
    pt.cex <- rep_len(pt.cex, n.leg)
    pt.lwd <- rep_len(pt.lwd, n.leg)
    ok <- !is.na(pch)
    if (!is.character(pch)) {
      ok <- ok & (pch >= 0 | pch <= -32)
    }
    else {
      ok <- ok & nzchar(pch)
    }
    x1 <- (if (!end.pch) {## tmuench, 2023/08/01 ->
             (if (merge && do.lines) 
                xt - (seg.len/2) * xchar
              else xt)[ok]
           } else {
             (if (merge && do.lines) 
                xt - (seg.len) * xchar - (pch.xoff) * xchar
              else xt)[ok]
           })

    if (end.pch)
      x2 <- (xt + (pch.xoff) * xchar)[ok]
    ## <-
    y1 <- yt[ok]
    if (trace) 
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
           ", ...)")
    if (plot) {
      points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
              bg = pt.bg[ok], lwd = pt.lwd[ok])
      ## tmuench, 2023/08/23 ->
      if (end.pch)
        points2(x2, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
                bg = pt.bg[ok], lwd = pt.lwd[ok])
      ## <-
    }
  }
  xt <- xt + x.intersp * xc
  if (plot) {
    if (!is.null(title)) 
      text2(left + w * title.adj, top - ymaxtitle, labels = title, 
            adj = c(title.adj, 0), cex = title.cex, col = title.col, 
            font = title.font)
    text2(xt, yt, labels = legend, adj = adj, cex = cex, 
          col = text.col, font = text.font)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top), 
                 text = list(x = xt, y = yt)))
}
