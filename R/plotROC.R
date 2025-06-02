#' Plot ROC Curves for Classifier Evaluation
#'
#' @description
#' Creates Receiver Operating Characteristic (ROC) curves for each cell type
#' to visualize the classifier's performance. The function can plot individual
#' ROC curves for each class or a combined plot with all curves.
#'
#' @param evaluation_results List containing evaluation metrics from evaluateClassifier
#' @param plot_type Character string specifying the type of plot:
#'   \itemize{
#'     \item "individual": Separate plot for each cell type
#'     \item "combined": All ROC curves on one plot
#'   }
#' @param main Character string for the plot title
#' @param colors Character vector of colors for the ROC curves
#' @param legend_position Character string specifying legend position
#'
#' @return A ggplot object containing the ROC curve(s)
#'
#' @details
#' The function creates ROC curves that show:
#' \itemize{
#'   \item True Positive Rate (Sensitivity) vs False Positive Rate (1-Specificity)
#'   \item Area Under the Curve (AUC) for each cell type
#'   \item Optimal classification thresholds
#' }
#'
#' @examples
#' # Evaluate classifier
#' results <- evaluateClassifier(predictions, true_labels)
#'
#' # Plot individual ROC curves
#' plotROC(
#'   results,
#'   plot_type = "individual",
#'   main = "ROC Curves by Cell Type"
#' )
#'
#' # Plot combined ROC curves
#' plotROC(
#'   results,
#'   plot_type = "combined",
#'   main = "Combined ROC Curves",
#'   colors = rainbow(length(unique(true_labels)))
#' )
#'
#' @importFrom ggplot2 ggplot geom_line geom_abline labs theme_bw
#' @importFrom pROC roc
#'
#' @export
plotROC <- function(evaluation_results,
                   plot_type = "combined",
                   main = "ROC Curves",
                   colors = NULL,
                   legend_position = "bottom") {
  # Input validation
  if (!is.list(evaluation_results)) {
    stop("evaluation_results must be a list")
  }
  
  if (!plot_type %in% c("individual", "combined")) {
    stop("plot_type must be either 'individual' or 'combined'")
  }
  
  if (!is.character(main) || length(main) != 1) {
    stop("main must be a single character string")
  }
  
  if (!is.null(colors) && !is.character(colors)) {
    stop("colors must be a character vector")
  }

  # Get ROC data
  roc_data <- evaluation_results$rocObjList
  
  # Create plot
  if (plot_type == "individual") {
    # Create individual plots for each cell type
    plots <- lapply(names(roc_data), function(type) {
      roc_obj <- roc_data[[type]]
      ggplot(data = data.frame(
        fpr = 1 - roc_obj$specificities,
        tpr = roc_obj$sensitivities
      )) +
        geom_line(aes(x = fpr, y = tpr)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        labs(
          title = paste(main, "-", type),
          x = "False Positive Rate",
          y = "True Positive Rate"
        ) +
        theme_bw()
    })
    return(plots)
  } else {
    # Create combined plot
    plot_data <- do.call(rbind, lapply(names(roc_data), function(type) {
      roc_obj <- roc_data[[type]]
      data.frame(
        fpr = 1 - roc_obj$specificities,
        tpr = roc_obj$sensitivities,
        type = type
      )
    }))
    
    p <- ggplot(plot_data, aes(x = fpr, y = tpr, color = type)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(
        title = main,
        x = "False Positive Rate",
        y = "True Positive Rate"
      ) +
      theme_bw() +
      theme(legend.position = legend_position)
    
    if (!is.null(colors)) {
      p <- p + scale_color_manual(values = colors)
    }
    
    return(p)
  }
}
