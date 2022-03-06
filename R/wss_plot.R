#' @title Within groups sum of squares plot
#'
#' @description
#' Within Groups Sum of Squares Plot
#'
#' @details
#' \code{wss_plot} generates a plot of within-groups
#' sums-of-squares vs. number of clusters based on
#' k-means clustering. The clustering uses euclidean distances
#' between observations. By default, the variables are standardized
#' (recommended). The plot is useful for determining the number of clusters
#' present in the data. Look for the point were adding clusters does not significantly
#' decrease the within-groups sum-of-squares.
#'
#' @param data a data frame of numeric variables.
#' @param nc integer. The largest number of clusters to evaluate.
#' @param standardize logical. If \code{TRUE}, standardize data before
#' clustering.
#' @param seed integer. A random number seed for reproducablility.
#'
#' @return a ggplot2 graph
#' @import ggplot2
#' @importFrom stats kmeans
#' @export
#' @examples
#' # iris example (should find 3 clusters)
#' data(iris)
#' wss_plot(iris[-5])

wss_plot <- function(data, nc=15, standardize=TRUE, seed=1234){

  cluster <- NULL # for CRAN

  # standardize data
  if(!any(sapply(data, is.numeric)))stop("data must be numeric.")
  if (standardize){
     data <- standardize(data)
  }

  wss <- numeric(nc)
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  results <- data.frame(cluster=1:nc, wss=wss)
  ggplot(results, aes(x=cluster,y=wss)) +
    geom_point(color="steelblue", size=2) +
    geom_line(color="grey") +
    theme_bw() +
    labs(title="WSS Plot for Determining the Number of Clusters",
         x="Number of Clusters",
         y="Within groups sum of squares")
}
