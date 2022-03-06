#' Plot a Factor Solution
#'
#' This function plots the pattern matrix for a principal components or
#' common factor analysis solution
#'
#' @param x an object of class \code{factorAnalysis} produced by the \code{\link{PCA}}
#' or \code{\link{FA}} functions.
#' @param sort logical. If \code{TRUE}, sort the pattern matrix.
#' @param type generate a bar plot (\code{"bar"}) or table plot (\code{"table"}).
#' @param y not used
#' @param ... not used
#' @export
#' @return a ggplot2 graph
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom scales muted
#' @examples
#' fit.pca <- PCA(Harman74.cor$cov, nfactors=4, rotate="varimax")
#' plot(fit.pca, sort=TRUE)
#' plot(fit.pca, sort=TRUE, type="table")
plot.factorAnalysis <- function(x, y, ..., sort=FALSE, type=c("bar", "table")) {
  if (!inherits(x, "factorAnalysis")){
    stop("x must be of class factorAnalysis", call. = FALSE)
  }

  value <- key <- NULL # for CRAN

  type <- match.arg(type)
  x <- x$loadings
  factors <- ncol(x)

  # function to sort loadings
  sort_loadings <- function(x){
    mx <- max.col(abs(x))
    xlist <- split(x, mx)
    lnames <- names(xlist)
    result <- NULL
    for(i in lnames){
      df <- xlist[[i]]
      varnum <- as.numeric(i)
      ord <- order(-abs(df[, varnum]))
      df <- df[ord, ]
      result <- rbind(result, df)
    }
    return(result)
  }

  if (sort == TRUE) {
    x <- sort_loadings(x)
  }

  #x <- as.data.frame(x)
  x$var <- row.names(x)

  facorder <- rev(x$var)
  x <- gather(x, key="key", value="value", 1:factors)
  x$var <- factor(x$var, levels=facorder)

  if (type == "bar"){
    p <- ggplot(x, aes(var, abs(value), fill=value)) +
      facet_wrap(~ key, nrow=1) + #place the factors in separate facets
      geom_bar(stat="identity") + #make the bars
      coord_flip() + #flip the axes so the test names can be horizontal
      #define the fill color gradient: blue=positive, red=negative
      scale_fill_gradient2(name = "Loading",
                           high = "blue", mid = "white", low = "red",
                           midpoint=0, guide="none") +
      ylab("Loading Strength") + #improve y-axis label
      xlab("Variables")+
      theme_bw(base_size=10) #use a black-and-white theme with set font size
  }

  if (type == "table"){
    p <- ggplot(x, aes(key, var)) +
      geom_tile(aes(fill = value)) +
      geom_text(aes(label = round(value, 2)), size=3) +
      scale_fill_gradientn(colors = c( "red",
                                       "white",
                                       "blue"),
                           limits = c(-1, 1)) +
      theme_minimal()+
      theme(panel.grid.major=element_blank()) +
      labs(title = "Pattern Matrix",
           fill="Coefficient",
           x = "",
           y = "")

  }

  p
}
