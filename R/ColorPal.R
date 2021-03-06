#' ColorBrewer colour palettes
#'
#' Obtain a colour palette from the ColorBrewer 2.0 collection for use in
#' standard plots, image plots, or filled contour plots.
#'
#' @param name a palette name from the ColorBrewer 2.0 collection; defaults to
#'   the diverging \code{RdYlBu} palette.
#' @param n.in integer number of different input colours in the palette, minimum
#'   is 3, the possible maximum number of colours depends on the chosen
#'   palette. Default \code{NULL} means to use this maximum number of colours.
#' @param n.out integer number of colours for the output vector if \code{fun =
#'   FALSE}; the default is to output as many colours as used for the palette,
#'   so \code{n.out} is set to the value of \code{n.in}. If a different number
#'   is specified here, the corresponding colours are obtained from
#'   interpolation between the range of colours spanned by the palette for the
#'   given number \code{n.in}; see the examples.
#' @param rev logical; set to \code{TRUE} to reverse the order of colours in the
#'   palette.
#' @param fun logical to control the return type; if set to \code{FALSE} (the
#'   default) returned is simply a character vector of length \code{n.out} with
#'   the hexadecimal colour codes from the requested palette. Set to \code{TRUE}
#'   to instead obtain a function that takes an integer argument (the required
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
#' # return instead a colour palette function for use in filled contour plots
#' fun <- ColorPal(fun = TRUE)
#' fun(5)
#'
#' # note that the following two colour vectors are not identical:
#' ColorPal(n.in = 11, n.out = 3)
#' ColorPal(n.in = 3)
#' # while the first call gives three colours from interpolating between the
#' # full range of colours for the given palette (so that in this example the
#' # first and last returned colour correspond to the outermost colours in the
#' # given palette and the second colour is some midpoint value), the second
#' # result is a subset of three colours from the specified palette, which do
#' # not necessarily span the possible palette range and which are determined by
#' # the 'RColorBrewer::brewer.pal()' function. In this context, also compare
#' # the above results to the following call:
#' ColorPal(n.in = 3, n.out = 11)
#'
#' @export
ColorPal <- function(name = "RdYlBu", n.in = NULL, n.out = NULL,
                     rev = FALSE, fun = FALSE) {

  i <- match(name, rownames(RColorBrewer::brewer.pal.info))

  if (is.na(i)) stop("Unknown ColorBrewer palette.", call. = FALSE)

  if (!length(n.in))  n.in  <- RColorBrewer::brewer.pal.info$maxcolors[i]
  if (!length(n.out)) n.out <- n.in

  cols <- RColorBrewer::brewer.pal(n.in, name)
  if (rev) cols <- rev(cols)

  pal <- grDevices::colorRampPalette(cols)

  if (fun) {
    return(pal)
  } else {
    return(pal(n.out))
  }
}
