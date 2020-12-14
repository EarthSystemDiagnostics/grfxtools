#' @title Calculate and plot a pairwise correlations
#' @description Takes a matrix or dataframe and calculates the pairwise
#'   correlations between all columns. Plots these as a ggplot tilemap.
#'   Optionally returns the pairwise correlations as a tidy tibble.
#' @param M A matrix or dataframe for which the pairwise corrlelations between
#'   the columns are desired.
#' @param axis.label Optional label for x,y figure axes, Default: NULL
#' @param return.corr.tibble Return the correlations?, Default: FALSE
#' @param plotit Plot the correlations, Default: TRUE
#' @return Default: a ggplot2 object, optionally the correlations as a tibble.
#' @author Andrew Dolman
#' @details Ideas for code taken from here:
#'   http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
#'
#'
#' @examples
#' \dontrun{
#' if(interactive()) {
#'
#'   M <- matrix(rnorm(12 * 100), ncol = 12)
#'   PlotPairwiseCorrelations(M, axis.label = "Month",
#'                            return.corr.tibble = TRUE)
#'
#'   # Add names to columns
#'   colnames(M) <- letters[1:12]
#'   PlotPairwiseCorrelations(M, axis.label = "Cat",
#'                            return.corr.tibble = TRUE)
#'
#' }
#' }
#' @rdname CorrelationMatrix
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2
#'   scale_x_discrete scale_y_discrete theme_minimal coord_fixed
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
PlotPairwiseCorrelations <- function(M,
                                     axis.label = NULL,
                                     return.corr.tibble = FALSE,
                                     plotit = TRUE){
  if (is.null(colnames(M))) colnames(M) <- 1:ncol(M)
  c.m <- cor(M)
  c.m[lower.tri(c.m, diag = TRUE)] <- NA
  c.m <- tibble::as_tibble(c.m)
  c.m <- dplyr::mutate(c.m, fac.a = colnames(M))
  c.m <- tidyr::gather(c.m, fac.b, Correlation, -fac.a)
  c.m <- dplyr::mutate(c.m,
                       fac.a = factor(fac.a, levels = colnames(M), ordered = TRUE),
                       fac.b = factor(fac.b, levels = rev(colnames(M)), ordered = TRUE))

  c.m <- dplyr::filter(c.m, complete.cases(Correlation))

  p <- NULL

  if (plotit){
    p <- ggplot2::ggplot(c.m, ggplot2::aes(x = fac.a, y = fac.b, fill = Correlation))
    p <- p + ggplot2::geom_tile(colour = "white")
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)),
                                color = "black", size = 4)
    p <- p + ggplot2::scale_fill_gradient2(low = "blue", high = "yellow",
                                  mid = "white", midpoint = 0,
                                  limit = c(-1,1),
                                  name="Pearson\nCorrelation")
    p <- p + ggplot2::scale_x_discrete(axis.label)
    p <- p + ggplot2::scale_y_discrete(axis.label)
    p <- p + ggplot2::theme_minimal()
    p <- p + ggplot2::coord_fixed()
    p
  }

  if (return.corr.tibble){
    return(list(plot = p, correlations = c.m))
  }else{
      return(p)
    }

}
