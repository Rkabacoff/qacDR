#' Scree Test
#'
#' \code{screePlot} generates a scree plot with superimpose parallel analysis
#'
#' @param data a data frame or correlation matrix.
#' @param n.iter number of iterations for parallel analysis.
#' @param method factoring method (\code{pc}, \code{pa}, or \code{ml}).
#' @param ... parameters passed to \code{psych::fa.parallel}.
#'
#' @details
#' The \code{screePlot} function is a wrapper for the \code{psych::parallel} function.
#' A ggplot2 graph is produced with the scree plot and parallel analysis (scree plot for
#' simulated data) based on the method chosen. The methods are principal components ("pc"),
#' principal axis ("pa"), and maximum likelihood ("ml") factoring.
#'
#' @importFrom psych fa.parallel
#' @importFrom grDevices dev.off pdf
#' @import ggplot2
#'
#' @export
#'
#' @return a ggplot2 graph
#' @examples
#' screePlot(Harman74.cor$cov, method="pc", n.obs=145)
screePlot <- function(data, n.iter = 50, method = c("pc", "pa", "ml"), ...) {

  n <- type <- NULL # for CRAN
  # number of variables
  nvar <- ncol(data)

  # suppress output
  pdf(file=NULL)
  sink(file=tempfile())
  method <- match.arg(method)
  if (method == "pc") {
    x <- psych::fa.parallel(x = data, n.iter = n.iter,
                            fa = "pc", ...)
    caption <- "Principal Components Analysis"
  }

  if (method == "pa") {
    x <- psych::fa.parallel(x = data, n.iter = n.iter,
                            fa = "fa", fm ="pa", ...)
    caption <- "Principal Axis Factoring"
  }

  if (method == "ml") {
    x <- psych::fa.parallel(x = data, n.iter = n.iter,
                            fa = "fa", fm ="ml", ...)
    caption <- "Maximum Likelihood Factoring"
  }

  # unsuppress output
  sink()
  dev.off()

  if (method == "pc") {
    xlab <- "Component Number"
    yintercept <- 1
    actual <- x$pc.values
    sim <- x$pc.sim
    nfact <- x$ncomp
    #cat("\nNote: parallel analysis suggests", nfact, "components.\n")
  } else {
    xlab <- "Factor Number"
    yintercept <- 0
    actual <- x$fa.values
    sim <- x$fa.sim
    nfact <- x$nfact
    #cat("\nNote: parallel analysis suggests", nfact, "factors.\n")
  }


  # arrange plot data
  actual <- data.frame(n = 1:nvar, eigenvalue <- actual, type="actual")
  sim    <- data.frame(n = 1:nvar, eigenvalue <- sim, type="simulated")
  names(actual) <- names(sim) <- c("n", "eigenvalue", "type")
  plotdata <- as.data.frame(rbind(actual, sim))

  # plot results
  apatheme=theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          #text=element_text(family='Arial'),
          legend.title=element_blank(),
          legend.position=c(.7,.8),
          axis.line.x = element_line(color='black'),
          axis.line.y = element_line(color='black'))

  # Use data from plotdata. Map number of factors to x-axis, eigenvalue to y-axis,
  # and give different data point shapes depending on whether eigenvalue is
  # observed or simulated

  p = ggplot(plotdata, aes(x=n, y=eigenvalue, shape=type, color=type)) +

    # Add lines connecting data points
    geom_line() +

    # Add the data points.
    geom_point(size=2) +

    # Label axes and titles
    labs(y = "Eigenvalue",
         x = xlab,
         title = "Scree Plot",
         subtitle = paste0("with parallel analysis (", n.iter, " iterations)"),
         caption = caption) +

    # Ensure x-axis ranges from 1 to max # of factors,
    # increasing by one with each 'tick' mark.
    scale_x_continuous(breaks=min(plotdata$n):max(plotdata$n)) +

    # Ensure y-axis  that it ranges from below the lowest eigenvalue
    #  to above the highest eigenvalue an increments by .25.
    scale_y_continuous(breaks=round(seq(from = floor(min(plotdata$eigenvalue)),
                                        to = ceiling(max(plotdata$eigenvalue)),
                                        by=.5), 2)) +

    # Manually specify the different shapes to use for actual and simulated data,
    # in this case, white and black circles.
    scale_shape_manual(values=c(16, 1)) +

    # Add horizontal line indicating Kaiser Harris criteria
    geom_hline(yintercept = yintercept, linetype = "dashed", color="lightgray") +

    # Add vertical line indicating parallel analysis suggested max # of factors to retain
    #geom_vline(xintercept = nfact, linetype = 'dashed') +

    # Apply our apa-formatting theme
    apatheme

  # return plot
  p

}
