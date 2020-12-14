## Helper functions for ggplot
 # Custom scales, themes, palettes etc.

#' Transform axis to reversed log scale
#'
#' Transform axis to reversed log scale.
#'
#' @param base base for the log transformation; default is to use natural
#'   logarithm (base exp(1)).
#' @seealso
#'  \code{\link[scales]{trans_new}},\code{\link[scales]{breaks_log}}
#' @source https://stackoverflow.com/a/11054781/1198125
#' @export
#' @importFrom scales trans_new log_breaks
reverselog_trans <- function(base = exp(1)) {

  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)

  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
                    scales::log_breaks(base = base),
                    domain = c(1e-100, Inf))
}

#' Plot all ggforce::facet_wrap_paginate pages
#'
#' A helper function to return all pages from ggforce::facet_wrap_paginate. It
#' automatically calculates the required number of pages.
#'
#' @param ggplot.obj a ggplot2 object.
#' @param facets faceting formula; see \code{\link[ggplot2]{facet_wrap}}.
#' @param nrow number of columns per page.
#' @param ncol number of rows per page.
#' @return A list of ggplot2 objects.
#' @importFrom ggforce facet_wrap_paginate
#' @examples
#' library(ggplot2)
#' dat <- expand.grid(x = 1:100, a = letters[1:11])
#' dat$y <- dat$x + rnorm(nrow(dat), 0, 10)
#' p <- ggplot(dat, aes(x = x, y = y)) +
#'   geom_point() +
#'   facet_wrap(~a)
#' p
#'
#' gg <- facet_wrap_paginate_auto(p,
#'                                facets = formula(~a),
#'                                nrow = 2, ncol = 2)
#' gg
#' @export
facet_wrap_paginate_auto <- function(ggplot.obj, facets, nrow, ncol) {

  built.plot <- ggplot_build(ggplot.obj)

  # Make robust to changes in built ggplot object stucture in dev vs. CRAN version
  if (is.null(built.plot$layout$panel_params) == FALSE) {
    n.pages <-
      ceiling(length(built.plot$layout$panel_params) / (nrow * ncol))
  } else if (is.null(built.plot$layout$panel_ranges) == FALSE) {
    n.pages <-
      ceiling(length(built.plot$layout$panel_ranges) / (nrow * ncol))
  } else {
    stop("Structure of built ggplot object has changed")
  }

  ggs <- lapply(1:n.pages, function(i) {
    ggplot.obj +
      ggforce::facet_wrap_paginate(
        facets,
        scales = "free_y",
        labeller = labeller(.multi_line = FALSE),
        ncol = ncol,
        nrow = nrow,
        page = i
      )
  })

  return(ggs)
}
