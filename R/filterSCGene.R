#' Filter Genes in Single-Cell Expression Data
#'
#' @description
#' Filters genes in single-cell RNA sequencing data based on expression characteristics.
#' The function implements a multi-step filtering process to identify genes that are
#' likely to be informative for cell type classification.
#'
#' @param expr A matrix or sparse matrix of normalized gene expression data
#' @param mu Numeric value specifying the minimum mean expression threshold
#' @param alpha1 Numeric value specifying the minimum proportion of cells expressing a gene
#' @param alpha2 Numeric value specifying the maximum proportion of cells expressing a gene
#' @param threshold Numeric value specifying the expression threshold for considering a gene as expressed
#'
#' @return A character vector of gene names that pass all filtering criteria
#'
#' @details
#' The filtering process includes the following steps:
#' \enumerate{
#'   \item Calculate mean expression for each gene
#'   \item Calculate proportion of cells expressing each gene
#'   \item Filter genes based on:
#'     \itemize{
#'       \item Minimum mean expression (mu)
#'       \item Minimum proportion of expressing cells (alpha1)
#'       \item Maximum proportion of expressing cells (alpha2)
#'       \item Expression threshold
#'     }
#' }
#'
#' @examples
#' # Load and normalize example data
#' data(reference_expr)
#' normalized_expr <- normalizeSCExp(reference_expr, dThresh = 0.25)
#'
#' # Filter genes
#' filtered_genes <- filterSCGene(
#'   normalized_expr,
#'   mu = 2,
#'   alpha1 = 0.1,
#'   alpha2 = 0.01,
#'   threshold = 0.25
#' )
#'
#' @importFrom Matrix rowMeans
#'
#' @export
filterSCGene <- function(expr, mu = 2, alpha1 = 0.1, alpha2 = 0.01, threshold = 0.25) {
  # Input validation
  if (!is.matrix(expr) && !inherits(expr, "Matrix")) {
    stop("expr must be a matrix or sparse matrix")
  }
  
  if (!is.numeric(mu) || length(mu) != 1 || mu < 0) {
    stop("mu must be a non-negative numeric value")
  }
  
  if (!is.numeric(alpha1) || length(alpha1) != 1 || alpha1 < 0 || alpha1 > 1) {
    stop("alpha1 must be a numeric value between 0 and 1")
  }
  
  if (!is.numeric(alpha2) || length(alpha2) != 1 || alpha2 < 0 || alpha2 > 1) {
    stop("alpha2 must be a numeric value between 0 and 1")
  }
  
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0) {
    stop("threshold must be a non-negative numeric value")
  }

  # Calculate mean expression
  mean_expr <- rowMeans(expr)
  
  # Calculate proportion of cells expressing each gene
  prop_expr <- rowMeans(expr > threshold)
  
  # Apply filtering criteria
  filtered_genes <- rownames(expr)[
    mean_expr >= mu &
    prop_expr >= alpha1 &
    prop_expr <= alpha2
  ]
  
  if (length(filtered_genes) == 0) {
    warning("No genes passed the filtering criteria. Consider relaxing the thresholds.")
  }
  
  return(filtered_genes)
}
