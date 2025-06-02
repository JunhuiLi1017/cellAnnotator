#' Predict Cell Types Using SVM Classifier
#'
#' @description
#' Predicts cell types for new cells using a trained SVM classifier. The function
#' can return both predicted cell types and probability estimates for each class.
#'
#' @param classifier A trained SVM classifier object
#' @param newdata A matrix of test data (cells in rows, features in columns)
#' @param probability Logical indicating whether to return probability estimates (default: TRUE)
#'
#' @return If probability is TRUE, returns a list containing:
#' \itemize{
#'   \item predictions: Factor vector of predicted cell types
#'   \item probabilities: Matrix of probability estimates for each class
#' }
#' If probability is FALSE, returns only the predictions vector.
#'
#' @details
#' The prediction process includes the following steps:
#' \enumerate{
#'   \item Validates input data and classifier
#'   \item Scales the input data using the same parameters as training
#'   \item Makes predictions using the SVM model
#'   \item Optionally computes probability estimates
#' }
#'
#' @examples
#' # Load and prepare test data
#' data(query_expr)
#' normalized_expr <- normalizeSCExp(query_expr, dThresh = 0.25)
#' test_data <- t(as.matrix(normalized_expr[feature_genes,]))
#'
#' # Make predictions
#' predictions <- predictWithclassifier(
#'   classifier,
#'   test_data,
#'   probability = TRUE
#' )
#'
#' # Access predictions and probabilities
#' cell_types <- predictions$predictions
#' prob_matrix <- predictions$probabilities
#'
#' @importFrom e1071 predict.svm
#'
#' @export
predictWithclassifier <- function(classifier, newdata, probability = TRUE) {
  # Input validation
  if (!inherits(classifier, "svm")) {
    stop("classifier must be an SVM model object")
  }
  
  if (!is.matrix(newdata)) {
    stop("newdata must be a matrix")
  }
  
  if (!is.logical(probability) || length(probability) != 1) {
    stop("probability must be a single logical value")
  }
  
  if (ncol(newdata) != length(classifier$SV[1,])) {
    stop("Number of features in newdata must match the training data")
  }

  # Make predictions
  pred <- predict(
    classifier,
    newdata,
    probability = probability
  )
  
  if (probability) {
    # Extract probabilities
    prob_matrix <- attr(pred, "probabilities")
    
    # Return both predictions and probabilities
    return(list(
      predictions = pred,
      probabilities = prob_matrix
    ))
  } else {
    # Return only predictions
    return(pred)
  }
}

