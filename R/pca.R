#' Principal Components Analysis
#'
#' \code{PCA} performs a principal components analysis
#'
#' @param data a data frame or correlation matrix.
#' @param nfactors nuber of factors to extract.
#' @param rotate factor rotation to perform.
#' @param digits number of digits to retain.
#' @param ... parameters passed to the \code{psych::principal} function.
#'
#' @details
#' The \code{PCA} function is a wrapper for the \code{psych::principal} function.
#' Component rotations include \code{none}, \code{varimax},
#' and \code{promax}.
#'
#' @export
#' @importFrom psych principal
#'
#' @return returns a list with 5 components:
#' \item{call}{the call}
#' \item{loadings}{structure matrix}
#' \item{variance}{variance accounted for}
#' \item{phi}{component intercorrelations for oblique rotations}
#' \item{scores}{component scores if factors are extracted from a data frame}
#' @examples
#' fit.pca <- PCA(Harman74.cor$cov, nfactors=4, rotate="varimax")

PCA <- function(data, nfactors=NULL, rotate="none", digits=2, ...) {


  # default number of factors
  if (is.null(nfactors)){
    nfactors <- ncol(data)
  }

  # title
  cat("\nPrincipal Components Analysis\n")
  cat("Number of Factors:", nfactors, "Rotation:", rotate, "\n")


  # factor loadings and communalities
  result <- psych::principal(data, rotate=rotate, nfactors=nfactors)
  loadings <- result$loadings
  class(loadings) <- "matrix"
  loadings <- as.data.frame(loadings)
  names(loadings) <- paste0("PC", 1:nfactors)
  loadings$h2 <- result$communality
  loadings <- round(loadings, digits)

  # are the components orthogonal
  Phi <- Structure <- NULL
  orthogonal <- 0
  if (is.null(result$Phi)) orthogonal <- 1

  if (orthogonal) {
    heading <- "\nComponent Structure\n"
  } else {
    heading <- paste0("\nComponent Pattern\n")
  }

  cat(heading)
  print(loadings)

  # factor structure
  if (!orthogonal){
    Structure <- result$Structure
    class(Structure) <- "matrix"
    Structure <- as.data.frame(Structure)
    names(Structure) <- paste0("F", 1:nfactors)
    Structure <- round(Structure, digits)
    cat("\nComponent Structure\n")
    print(Structure)
  }


  # eigenvalues table
  ss_table <- result$Vaccounted
  if(nfactors==1){
    ss_table <- rbind(ss_table, ss_table[2, ])
  }
  ss_table <- ss_table[c(1,2,3),]
  ss_table <- round(as.data.frame(ss_table), digits)
  names(ss_table) <- paste0("PC", 1:nfactors)
  row.names(ss_table) <- c("Variance", "Var Explained", "Cum Var Explained")
  cat("\n")
  print(ss_table)

  # factor intercorrelations for oblique rotations
  if (!orthogonal){
    cat("\nComponent Intercorrelations\n")
    Phi <- round(as.data.frame(result$Phi), digits)
    colnames(Phi) <- row.names(Phi) <- paste0("PC", 1:ncol(Phi))
    print(Phi)
  }

  # factor scores
  scores <- NULL
  if (!is.null(result$scores)){
    scores <- as.data.frame(result$scores)
    names(scores) <- paste0("PC", 1:nfactors)
  }


  pca_results <- list(call = match.call(),
                      loadings=loadings[-which(names(loadings) == "h2")],
                      variance=ss_table,
                      Stucture = Structure,
                      phi = Phi,
                      scores = scores)
  class(pca_results) <- "factorAnalysis"
  invisible(pca_results)
}


