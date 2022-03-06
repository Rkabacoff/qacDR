#' @title Cluster profile plots
#'
#' @description
#' \code{profile_plot} generates a mean profile plot for each cluster in
#' a cluster analysis.
#'
#' @details
#' The data frame must contain numeric variables for all variables except
#' the cluster variable. The cluster variable can be numeric, character, or
#' factor. The data will be standardized before plotting. The bar charts
#' are faceted, while the line charts are grouped.
#'
#' @param data a data frame.
#' @param cluster variable containing cluster membership.
#' @param type character. \code{"bar"} or \code{"line"} plots.
#'
#' @return a ggplot2 graph
#' @export
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @examples
#' # 3 cluster solution
#' iris <- standardize(iris)
#' df <- iris[-5]
#' set.seed(1234)
#' fit <- kmeans(df, 3)
#' df$cluster <- fit$cluster
#' profile_plot(df, type="bar")
#' profile_plot(df, type="line")
profile_plot <- function(data, cluster="cluster",
                         type=c("bar", "line")){

  type=match.arg(type)

  data <- as.data.frame(data)
  if (is.numeric(data[[cluster]])){
    clustervar <- factor(data[[cluster]],
                         labels=paste("cluster",
                                      levels(data[[cluster]])))
  } else {
    clustervar <- factor(data[[cluster]])
  }

  data[cluster] <- NULL
  data <- standardize(data)
  data$cluster <- clustervar


  profiles <- data %>%
    group_by(cluster)%>%
    summarize_all(mean) %>%
    data.frame()

  plotdata <- gather(profiles, key="variable",
                     value="measurement",
                     -cluster)

  if(type=="bar"){
    p <- ggplot(plotdata,
         aes(x=.data[["variable"]],
             y=.data[["measurement"]],
             fill=.data[["variable"]],
             group=.data[["cluster"]])) +
    geom_bar(stat="identity") +
    geom_hline(yintercept=0) +
    facet_wrap(~cluster) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, vjust=0),
          legend.position="none") +
    labs(x="", y="Standardized scores",
         title = "Mean Cluster Profiles")


  }
  if(type=="line"){
      p <- ggplot(plotdata,
             aes(x=.data[["variable"]],
                 y=.data[["measurement"]],
                 color=.data[["cluster"]],
                 group=.data[["cluster"]])) +
        geom_point(size=3) +
        geom_line(size=1) +
        geom_hline(yintercept=0) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=90, vjust=0)) +
        labs(x="", y="Standardized scores",
             title = "Mean Cluster Profiles")
  }

  return(p)
}
