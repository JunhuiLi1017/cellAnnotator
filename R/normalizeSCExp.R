#' Normalize Single-Cell Expression Data
#'
#' @description
#' Normalizes single-cell RNA sequencing expression data using a combination of
#' library size normalization and log transformation. The function handles both
#' dense and sparse matrices efficiently.
#'
#' @param expr A matrix or sparse matrix of gene expression data (genes in rows, cells in columns)
#' @param dThresh Numeric value specifying the minimum expression threshold for normalization
#' @param scale Logical indicating whether to scale the data after normalization
#'
#' @return A normalized matrix or sparse matrix with the same dimensions as the input
#'
#' @details
#' The normalization process includes the following steps:
#' \enumerate{
#'   \item Library size normalization (counts per million)
#'   \item Log transformation (log2(x + 1))
#'   \item Optional scaling to unit variance
#' }
#'
#' The function handles both dense and sparse matrices efficiently and preserves
#' the matrix type of the input data.
#'
#' @examples
#' # Load example data
#' data(reference_expr)
#'
#' # Normalize expression data
#' normalized_expr <- normalizeSCExp(
#'   reference_expr,
#'   dThresh = 0.25,
#'   scale = TRUE
#' )
#'
#' @importFrom Matrix Matrix
#' @importFrom methods as
#'
#' @export
normalizeSCExp <- function(expr, dThresh = 0.25, scale = TRUE) {
  # Input validation
  if (!is.matrix(expr) && !inherits(expr, "Matrix")) {
    stop("expr must be a matrix or sparse matrix")
  }
  
  if (!is.numeric(dThresh) || length(dThresh) != 1 || dThresh < 0) {
    stop("dThresh must be a non-negative numeric value")
  }
  
  if (!is.logical(scale) || length(scale) != 1) {
    stop("scale must be a single logical value")
  }

  # Convert to sparse matrix if needed
  if (!inherits(expr, "Matrix")) {
    expr <- Matrix(expr, sparse = TRUE)
  }

  # Library size normalization
  col_sums <- colSums(expr)
  expr_norm <- t(t(expr) / col_sums) * 1e6

  # Log transformation
  expr_norm <- log2(expr_norm + 1)

  # Apply threshold
  expr_norm[expr_norm < dThresh] <- 0

  # Scale if requested
  if (scale) {
    gene_means <- rowMeans(expr_norm)
    gene_sds <- sqrt(rowVars(expr_norm))
    expr_norm <- (expr_norm - gene_means) / gene_sds
  }

  return(expr_norm)
}
