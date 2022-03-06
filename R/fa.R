#' Factor Analysis
#'
#' \code{FA} performs common factor analysis
#'
#' @param data a data frame or correlation matrix.
#' @param nfactors nuber of factors to extract.
#' @param rotate factor rotation to perform.
#' @param fm type of factor extraction.
#' @param digits number of digits to retain.
#' @param ... parameters passed to the \code{psych::fa} function.
#'
#' @details
#' The \code{FA} function is a wrapper for the \code{psych:fa} function.
#' Factor extractions include principal axis (\code{pa}) and maximum likelihood
#' (\code{ml}), among others. Factor rotations include \code{none}, \code{varimax},
#' and \code{promax}.
#'
#' @export
#' @importFrom psych fa
#'
#' @return returns a list with 5 components:
#' \item{call}{the call}
#' \item{loadings}{factor pattern}
#' \item{variance}{variance accounted for}
#' \item{Structure}{structure matrix}
#' \item{phi}{factor intercorrelations for oblique rotations}
#' \item{scores}{factor scores if factors are extracted from a data frame}
#' @examples
#' fit.fa <- FA(Harman74.cor$cov, nfactors=4, rotate="varimax")
FA <- function(data, nfactors=NULL, rotate="none", fm="pa", digits=2, ...) {

  # default number of factors
  if (is.null(nfactors)){
    nfactors <- ncol(data)
  }

  # title
  method <- fm
  if (fm == "pa"){
    method <- "Principal Axis"
  } else if (fm == "ml"){
    method <- "Maximum Likelihood"
  } else if (fm == "wls"){
    method <- "Weighted Least Squares"
  } else if (fm == "gls") {
    method <- "Generalized Least Squares"
  } else if (fm == "minres"){
    method <- "Minimum Residuals"
  }
  title <- paste(method, "Factor Analysis\n")
  cat("\n")
  cat(title)
  cat("Number of Factors:", nfactors, "/ Rotation:", rotate, "\n")

  result <- suppressMessages(suppressWarnings(
    psych::fa(data, nfactors=nfactors, rotate=rotate, fm=fm, ...)))

  # factor pattern
  loadings <- result$loadings
  class(loadings) <- "matrix"
  loadings <- as.data.frame(loadings)
  names(loadings) <- paste0("F", 1:nfactors)
  loadings$h2 <- result$communality
  loadings <- round(loadings, digits)

  cat("\nFactor Pattern\n")
  print(loadings)

  # factor structure
  Structure <- NULL
  if (!is.null(result$Phi)){
    # factor structure
    Structure <- result$Structure
    class(Structure) <- "matrix"
    Structure <- as.data.frame(Structure)
    names(Structure) <- paste0("F", 1:nfactors)
    Structure <- round(Structure, digits)
    cat("\nFactor Structure\n")
    print(Structure)
  }


  # eigenvalues table
  ss_table <- result$Vaccounted
  if(nfactors==1){
    ss_table <- rbind(ss_table, ss_table[2, ])
  }
  ss_table <- round(as.data.frame(ss_table), digits)
  names(ss_table) <- paste0("F", 1:nfactors)
  cat("\n")
  print(ss_table)

  # factor intercorrelations for oblique rotations
  Phi <- NULL
  if (!is.null(result$Phi)){
    cat("\nFactor Intercorrelations\n")
    Phi <- round(as.data.frame(result$Phi), digits)
    colnames(Phi) <- row.names(Phi) <- paste0("F", 1:ncol(Phi))
    print(Phi)
  }

  # factor scores
  scores <- NULL
  if (!is.null(result$scores)){
    scores <- as.data.frame(result$scores)
    names(scores) <- paste0("F", 1:nfactors)
  }

  factor_results <- list(call = match.call(),
                      loadings=loadings[-which(names(loadings) == "h2")],
                      variance=ss_table,
                      Structure = Structure,
                      phi = Phi,
                      scores = scores)
  class(factor_results) <- "factorAnalysis"
  invisible(factor_results)
}



