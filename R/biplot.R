#' Principal Components Biplot
#'
#' \code{biPlot} performs a principal components analysis and displays the variables
#' and observations in the space of the first two principal components.
#'
#' @param data a data frame.
#' @param ... parameters passed to the \code{factoextra::fviz_pca_biplot} function.
#'
#' @details
#' The \code{biPlot} function is a wrapper for the
#' \code{\link[stats]{prcomp}} and
#' \code{\link[factoextra]{fviz_pca_biplot}} functions. For the former,
#' \code{center=TRUE} and \code{scale=TRUE} are set. For the later,
#' \code{repel=TRUE} is set.
#'
#' @export
#' @importFrom factoextra fviz_pca_biplot
#' @importFrom stats prcomp var
#'
#'
#' @return a ggplot2 graph
#'
#' @examples
#' biPlot(USArrests)

biPlot <- function(data, ...) {

  if (!inherits(data, "data.frame")){
    stop("x must be a data.frame", call. = FALSE)
  }

  fit <- prcomp(data, center=TRUE, scale=TRUE)
  factoextra::fviz_pca_biplot(fit, repel=TRUE, ...)

}


