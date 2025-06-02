#' Plot Feature Genes for Cell Type Classification
#'
#' @description
#' Creates visualizations of feature genes that are most important for
#' distinguishing between cell types. The function can generate either
#' a heatmap or a boxplot to show the expression patterns of these genes
#' across different cell types.
#'
#' @param expression_matrix Matrix of normalized gene expression values
#' @param cell_types Factor vector of cell type labels
#' @param feature_genes Character vector of gene names to plot
#' @param plot_type Character string specifying the type of plot:
#'   \itemize{
#'     \item "heatmap": Shows expression patterns across all cells
#'     \item "boxplot": Shows distribution of expression for each cell type
#'   }
#' @param main Character string for the plot title
#' @param color_palette Character string specifying the color palette to use
#' @param show_cell_names Logical indicating whether to show cell names in heatmap
#' @param cluster_cells Logical indicating whether to cluster cells in heatmap
#'
#' @return A ggplot object containing the feature gene visualization
#'
#' @details
#' The function provides two visualization options:
#' \itemize{
#'   \item Heatmap: Shows expression patterns of feature genes across all cells,
#'     with cells grouped by cell type
#'   \item Boxplot: Shows the distribution of expression for each feature gene
#'     across different cell types
#' }
#'
#' @examples
#' # Get feature genes
#' feature_genes <- findFeatureGene(expression_matrix, cell_types)
#'
#' # Plot heatmap of feature genes
#' plotFeatureGenes(
#'   expression_matrix,
#'   cell_types,
#'   feature_genes,
#'   plot_type = "heatmap",
#'   main = "Feature Gene Expression Patterns"
#' )
#'
#' # Plot boxplot of feature genes
#' plotFeatureGenes(
#'   expression_matrix,
#'   cell_types,
#'   feature_genes,
#'   plot_type = "boxplot",
#'   main = "Feature Gene Expression by Cell Type"
#' )
#'
#' @importFrom ggplot2 ggplot geom_tile geom_boxplot scale_fill_gradient labs theme_bw
#' @importFrom pheatmap pheatmap
#' @importFrom reshape2 melt
#'
#' @export
plotFeatureGenes <- function(expression_matrix,
                           cell_types,
                           feature_genes,
                           plot_type = "heatmap",
                           main = "Feature Genes",
                           color_palette = "RdBu",
                           show_cell_names = FALSE,
                           cluster_cells = TRUE) {
  # Input validation
  if (!is.matrix(expression_matrix)) {
    stop("expression_matrix must be a matrix")
  }
  
  if (!is.factor(cell_types)) {
    stop("cell_types must be a factor")
  }
  
  if (!is.character(feature_genes)) {
    stop("feature_genes must be a character vector")
  }
  
  if (!plot_type %in% c("heatmap", "boxplot")) {
    stop("plot_type must be either 'heatmap' or 'boxplot'")
  }
  
  if (!is.character(main) || length(main) != 1) {
    stop("main must be a single character string")
  }
  
  if (!is.logical(show_cell_names) || length(show_cell_names) != 1) {
    stop("show_cell_names must be a single logical value")
  }
  
  if (!is.logical(cluster_cells) || length(cluster_cells) != 1) {
    stop("cluster_cells must be a single logical value")
  }

  # Subset expression matrix to feature genes
  expr_subset <- expression_matrix[feature_genes, ]
  
  if (plot_type == "heatmap") {
    # Create annotation for cell types
    annotation_col <- data.frame(
      CellType = cell_types,
      row.names = colnames(expr_subset)
    )
    
    # Create heatmap
    pheatmap(
      expr_subset,
      main = main,
      color = colorRampPalette(brewer.pal(9, color_palette))(100),
      show_rownames = TRUE,
      show_colnames = show_cell_names,
      cluster_rows = TRUE,
      cluster_cols = cluster_cells,
      annotation_col = annotation_col
    )
  } else {
    # Prepare data for boxplot
    plot_data <- melt(
      expr_subset,
      varnames = c("Gene", "Cell"),
      value.name = "Expression"
    )
    plot_data$CellType <- cell_types[plot_data$Cell]
    
    # Create boxplot
    ggplot(plot_data, aes(x = CellType, y = Expression, fill = CellType)) +
      geom_boxplot() +
      facet_wrap(~ Gene, scales = "free_y") +
      labs(
        title = main,
        x = "Cell Type",
        y = "Expression"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  }
} 