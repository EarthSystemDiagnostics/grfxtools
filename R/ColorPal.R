#' ColorBrewer colour palettes
#'
#' Obtain a colour palette from the ColorBrewer 2.0 collection for use in
#' standard plots, image plots, or filled contour plots.
#'
#' @param name a palette name from the ColorBrewer 2.0 collection; defaults to
#'   the diverging \code{RdYlBu} palette.
#' @param n integer number of different colours in the palette, minimum 3,
#'   maximum depending on the chosen palette. Default is \code{10} for the
#'   \code{RdYlBu} palette.
#' @param rev logical; set to \code{TRUE} to reverse the order of colours in the
#'   palette.
#' @param fun logical to control the return type; if set to \code{FALSE} (the
#'   default) returned is simply a character vector of length \code{n} with the
#'   hexadecimal colour codes from the requested palette. Set to \code{TRUE} to
#'   instead obtain a function that takes an integer argument (the required
#'   number of colours) and returns a character vector of hexadecimal colour
#'   codes, which is needed, e.g., for a filled contour plot
#'   (\code{\link[graphics]{filled.contour}}).
#' @return either a character vector of hexadecimal colour codes, or a function
#'   to return a colour code vector.
#' @author Thomas Münch
#' @seealso \code{\link[grDevices]{colorRampPalette}};
#'   \code{\link[RColorBrewer]{brewer.pal}}
#' @source The ColorBrewer 2.0 collection can be viewed interactively at
#'   https://colorbrewer2.org.
#' @examples
#'
#' # default palette
#' ColorPal()
#'
#' # reverse the order of colours
#' ColorPal(rev = TRUE)
#'
#' # return a colour palette function instead for use in filled contour plots
#' fun <- ColorPal(fun = TRUE)
#' fun(5)
#'
#' @export
ColorPal <- function(name = "RdYlBu", n = 10, rev = FALSE, fun = FALSE) {

  cols <- RColorBrewer::brewer.pal(n, name)

  if (rev) cols <- rev(cols)

  if (fun) {
    return(grDevices::colorRampPalette(cols))
  } else {
    return(cols)
  }
}
