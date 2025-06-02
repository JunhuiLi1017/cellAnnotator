#' Plot Confusion Matrix for Classifier Evaluation
#'
#' @description
#' Creates a heatmap visualization of the confusion matrix to show the
#' performance of the classifier across different cell types. The plot
#' includes the number of cells and percentage for each cell type
#' combination.
#'
#' @param evaluation_results List containing evaluation metrics from evaluateClassifier
#' @param normalize Logical indicating whether to show percentages (TRUE) or counts (FALSE)
#' @param main Character string for the plot title
#' @param color_palette Character string specifying the color palette to use
#' @param font_size Numeric value for the font size of the labels
#'
#' @return A ggplot object containing the confusion matrix heatmap
#'
#' @details
#' The confusion matrix shows:
#' \itemize{
#'   \item True cell types on the y-axis
#'   \item Predicted cell types on the x-axis
#'   \item Color intensity representing the number/percentage of cells
#'   \item Actual values in each cell
#' }
#'
#' @examples
#' # Evaluate classifier
#' results <- evaluateClassifier(predictions, true_labels)
#'
#' # Plot confusion matrix with counts
#' plotConfusionMatrix(
#'   results,
#'   normalize = FALSE,
#'   main = "Confusion Matrix (Counts)"
#' )
#'
#' # Plot confusion matrix with percentages
#' plotConfusionMatrix(
#'   results,
#'   normalize = TRUE,
#'   main = "Confusion Matrix (Percentages)",
#'   color_palette = "Blues"
#' )
#'
#' @importFrom ggplot2 ggplot geom_tile geom_text scale_fill_gradient labs theme_bw
#' @importFrom pheatmap pheatmap
#'
#' @export
plotConfusionMatrix <- function(evaluation_results,
                              normalize = TRUE,
                              main = "Confusion Matrix",
                              color_palette = "Reds",
                              font_size = 10) {
  # Input validation
  if (!is.list(evaluation_results)) {
    stop("evaluation_results must be a list")
  }
  
  if (!is.logical(normalize) || length(normalize) != 1) {
    stop("normalize must be a single logical value")
  }
  
  if (!is.character(main) || length(main) != 1) {
    stop("main must be a single character string")
  }
  
  if (!is.numeric(font_size) || length(font_size) != 1 || font_size <= 0) {
    stop("font_size must be a positive numeric value")
  }

  # Get confusion matrix
  conf_matrix <- evaluation_results$confusionMatrix
  
  # Normalize if requested
  if (normalize) {
    conf_matrix <- prop.table(conf_matrix, margin = 1) * 100
    value_format <- "%.1f%%"
  } else {
    value_format <- "%d"
  }
  
  # Create plot
  pheatmap(
    conf_matrix,
    main = main,
    color = colorRampPalette(brewer.pal(9, color_palette))(100),
    display_numbers = TRUE,
    number_format = value_format,
    fontsize = font_size,
    cluster_rows = FALSE,
    cluster_cols = FALSE
  )
} 