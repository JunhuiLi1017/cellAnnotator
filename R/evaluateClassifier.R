#' Evaluate SVM Classifier Performance
#'
#' @description
#' Evaluates the performance of a trained SVM classifier using various metrics
#' including accuracy, precision, recall, F1 score, and confusion matrix.
#'
#' @param predictions A factor vector of predicted cell types
#' @param true_labels A factor vector of true cell type labels
#' @param probability_matrix Optional matrix of prediction probabilities
#'
#' @return A list containing:
#' \itemize{
#'   \item accuracy: Overall classification accuracy
#'   \item confusion_matrix: Confusion matrix of predictions vs true labels
#'   \item precision: Precision for each cell type
#'   \item recall: Recall for each cell type
#'   \item f1_score: F1 score for each cell type
#'   \item macro_avg: Macro-averaged metrics across all cell types
#'   \item micro_avg: Micro-averaged metrics across all cell types
#' }
#'
#' @details
#' The evaluation process includes the following steps:
#' \enumerate{
#'   \item Computes confusion matrix
#'   \item Calculates per-class metrics (precision, recall, F1)
#'   \item Computes macro and micro averages
#'   \item Optionally uses probability estimates for more detailed analysis
#' }
#'
#' @examples
#' # Make predictions
#' predictions <- predictWithclassifier(classifier, test_data)
#'
#' # Evaluate classifier
#' results <- evaluateClassifier(
#'   predictions$predictions,
#'   true_labels,
#'   predictions$probabilities
#' )
#'
#' # Access results
#' print(results$accuracy)
#' print(results$confusion_matrix)
#' print(results$macro_avg)
#'
#' @importFrom caret confusionMatrix
#' @importFrom stats aggregate
#'
#' @export
evaluateClassifier <- function(predictions, true_labels, probability_matrix = NULL) {
  # Input validation
  if (!is.factor(predictions)) {
    stop("predictions must be a factor")
  }
  
  if (!is.factor(true_labels)) {
    stop("true_labels must be a factor")
  }
  
  if (length(predictions) != length(true_labels)) {
    stop("Length of predictions must match length of true_labels")
  }
  
  if (!is.null(probability_matrix) && 
      (!is.matrix(probability_matrix) || nrow(probability_matrix) != length(predictions))) {
    stop("probability_matrix must be a matrix with rows matching predictions")
  }

  # Compute confusion matrix
  conf_matrix <- confusionMatrix(predictions, true_labels)
  
  # Calculate per-class metrics
  precision <- conf_matrix$byClass[, "Precision"]
  recall <- conf_matrix$byClass[, "Recall"]
  f1 <- conf_matrix$byClass[, "F1"]
  
  # Compute macro averages
  macro_avg <- list(
    precision = mean(precision, na.rm = TRUE),
    recall = mean(recall, na.rm = TRUE),
    f1 = mean(f1, na.rm = TRUE)
  )
  
  # Compute micro averages
  micro_avg <- list(
    precision = conf_matrix$overall["Accuracy"],
    recall = conf_matrix$overall["Accuracy"],
    f1 = conf_matrix$overall["Accuracy"]
  )
  
  # Return results
  list(
    accuracy = conf_matrix$overall["Accuracy"],
    confusion_matrix = conf_matrix$table,
    precision = precision,
    recall = recall,
    f1_score = f1,
    macro_avg = macro_avg,
    micro_avg = micro_avg
  )
}


