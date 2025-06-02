#' Split Reference Dataset into Training and Testing Sets
#'
#' @description
#' Splits a reference dataset into training and testing sets for cell type classification.
#' The function ensures balanced representation of cell types in both sets and maintains
#' the relationship between metadata and expression data.
#'
#' @param meta A data frame containing cell metadata, including cell type information
#' @param expr A matrix or sparse matrix of gene expression data (genes in rows, cells in columns)
#' @param groupBy Character string specifying the column name in meta containing cell type labels
#' @param sampleID Character string specifying the column name in meta containing cell IDs
#' @param splitParameter Character string specifying the splitting method:
#'   \itemize{
#'     \item "ratio": Split based on a fixed ratio (default: 0.7 for training)
#'     \item "number": Split based on a fixed number of cells per type
#'   }
#' @param trainRatio Numeric value between 0 and 1 specifying the proportion of cells for training
#'   (only used when splitParameter = "ratio")
#' @param trainNumber Integer specifying the number of cells per type for training
#'   (only used when splitParameter = "number")
#'
#' @return A list containing:
#' \itemize{
#'   \item trainResponseData: Data frame of training set metadata
#'   \item testResponseData: Data frame of testing set metadata
#'   \item trainExplanatoryData: Matrix of training set expression data
#'   \item testExplanatoryData: Matrix of testing set expression data
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input data and parameters
#'   \item Groups cells by cell type
#'   \item Splits each cell type group according to specified parameters
#'   \item Maintains correspondence between metadata and expression data
#'   \item Returns balanced training and testing sets
#' }
#'
#' @examples
#' # Load example data
#' data(reference_meta)
#' data(reference_expr)
#'
#' # Split data using ratio method
#' alldataset <- splitTrainTest(
#'   reference_meta,
#'   reference_expr,
#'   groupBy = "cellType",
#'   sampleID = "cell",
#'   splitParameter = "ratio",
#'   trainRatio = 0.7
#' )
#'
#' # Split data using number method
#' alldataset <- splitTrainTest(
#'   reference_meta,
#'   reference_expr,
#'   groupBy = "cellType",
#'   sampleID = "cell",
#'   splitParameter = "number",
#'   trainNumber = 100
#' )
#'
#' @export
splitTrainTest <- function(meta,
                          expr,
                          groupBy,
                          sampleID,
                          splitParameter = "ratio",
                          trainRatio = 0.7,
                          trainNumber = NULL) {
  # Input validation
  if (!is.data.frame(meta)) {
    stop("meta must be a data frame")
  }
  
  if (!is.matrix(expr) && !inherits(expr, "Matrix")) {
    stop("expr must be a matrix or sparse matrix")
  }
  
  if (!is.character(groupBy) || length(groupBy) != 1) {
    stop("groupBy must be a single character string")
  }
  
  if (!is.character(sampleID) || length(sampleID) != 1) {
    stop("sampleID must be a single character string")
  }
  
  if (!groupBy %in% colnames(meta)) {
    stop(sprintf("Column '%s' not found in meta data", groupBy))
  }
  
  if (!sampleID %in% colnames(meta)) {
    stop(sprintf("Column '%s' not found in meta data", sampleID))
  }
  
  if (!splitParameter %in% c("ratio", "number")) {
    stop("splitParameter must be either 'ratio' or 'number'")
  }
  
  if (splitParameter == "ratio" && (trainRatio <= 0 || trainRatio >= 1)) {
    stop("trainRatio must be between 0 and 1")
  }
  
  if (splitParameter == "number" && (is.null(trainNumber) || trainNumber <= 0)) {
    stop("trainNumber must be a positive integer when splitParameter is 'number'")
  }

  # Get unique cell types
  cellTypes <- unique(meta[[groupBy]])
  
  # Initialize lists to store split data
  trainMeta <- list()
  testMeta <- list()
  
  # Split data for each cell type
  for (type in cellTypes) {
    typeCells <- meta[meta[[groupBy]] == type, ]
    
    if (splitParameter == "ratio") {
      nTrain <- ceiling(nrow(typeCells) * trainRatio)
    } else {
      nTrain <- min(trainNumber, nrow(typeCells))
    }
    
    # Randomly select cells for training
    trainIndices <- sample(1:nrow(typeCells), nTrain)
    trainMeta[[type]] <- typeCells[trainIndices, ]
    testMeta[[type]] <- typeCells[-trainIndices, ]
  }
  
  # Combine results
  trainMeta <- do.call(rbind, trainMeta)
  testMeta <- do.call(rbind, testMeta)
  
  # Get corresponding expression data
  trainExpr <- expr[, trainMeta[[sampleID]]]
  testExpr <- expr[, testMeta[[sampleID]]]
  
  # Return results
  list(
    trainResponseData = trainMeta,
    testResponseData = testMeta,
    trainExplanatoryData = trainExpr,
    testExplanatoryData = testExpr
  )
}
