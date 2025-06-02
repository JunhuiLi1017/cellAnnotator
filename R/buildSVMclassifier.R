#' Build SVM Classifier for Cell Type Classification
#'
#' @description
#' Builds a Support Vector Machine (SVM) classifier for cell type classification
#' using normalized gene expression data. The function uses the e1071 package's
#' implementation of SVM with a radial basis function kernel.
#'
#' @param x A matrix of training data (cells in rows, features in columns)
#' @param y A factor vector of cell type labels
#' @param kernel Character string specifying the kernel type (default: "radial")
#' @param cost Numeric value specifying the cost parameter (default: 1)
#' @param gamma Numeric value specifying the gamma parameter (default: 1/ncol(x))
#' @param probability Logical indicating whether to compute probability estimates (default: TRUE)
#'
#' @return An SVM model object that can be used for prediction
#'
#' @details
#' The function builds an SVM classifier with the following characteristics:
#' \itemize{
#'   \item Uses radial basis function kernel by default
#'   \item Automatically scales the input features
#'   \item Computes probability estimates for predictions
#'   \item Optimizes the model for multi-class classification
#' }
#'
#' @examples
#' # Prepare training data
#' data(reference_expr)
#' normalized_expr <- normalizeSCExp(reference_expr, dThresh = 0.25)
#' filtered_genes <- filterSCGene(normalized_expr)
#' feature_genes <- findFeatureGene(responseData, normalized_expr, filtered_genes)
#'
#' # Build classifier
#' x <- t(as.matrix(normalized_expr[feature_genes,]))
#' y <- as.factor(cell_types)
#' classifier <- buildSVMclassifier(
#'   x,
#'   y,
#'   kernel = "radial",
#'   cost = 1,
#'   gamma = 1/ncol(x),
#'   probability = TRUE
#' )
#'
#' @importFrom e1071 svm
#'
#' @export
buildSVMclassifier <- function(x, y, kernel = "radial", cost = 1, 
                             gamma = 1/ncol(x), probability = TRUE) {
  # Input validation
  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }
  
  if (!is.factor(y)) {
    stop("y must be a factor")
  }
  
  if (nrow(x) != length(y)) {
    stop("Number of rows in x must match length of y")
  }
  
  if (!is.character(kernel) || length(kernel) != 1) {
    stop("kernel must be a single character string")
  }
  
  if (!is.numeric(cost) || length(cost) != 1 || cost <= 0) {
    stop("cost must be a positive numeric value")
  }
  
  if (!is.numeric(gamma) || length(gamma) != 1 || gamma <= 0) {
    stop("gamma must be a positive numeric value")
  }
  
  if (!is.logical(probability) || length(probability) != 1) {
    stop("probability must be a single logical value")
  }

  # Build SVM model
  model <- svm(
    x = x,
    y = y,
    kernel = kernel,
    cost = cost,
    gamma = gamma,
    probability = probability,
    scale = TRUE
  )
  
  return(model)
}
