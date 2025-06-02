#' Save a trained classifier to a file
#'
#' @description
#' Saves a trained SVM classifier along with its associated metadata to an RDS file.
#' The saved object includes:
#' \itemize{
#'   \item The SVM classifier model
#'   \item Feature genes used for classification
#'   \item Timestamp of when the classifier was saved
#'   \item Version of cellAnnotator used
#' }
#'
#' @param classifier A trained SVM classifier object
#' @param file Character string specifying the path where the classifier should be saved
#' @param feature_genes Optional character vector of feature genes used in the classifier.
#'   If not provided, will attempt to extract from the classifier object.
#' @param ... Additional arguments passed to \code{\link[base]{saveRDS}}
#'
#' @return Invisibly returns TRUE if successful
#'
#' @examples
#' \dontrun{
#' # After training a classifier with feature genes
#' uniqueFeatureGene <- findFeatureGene(responseData, trainNorExp, 
#'                                     filteredGene = filterGene,
#'                                     topX = 20, reverse = TRUE)
#' x <- t(as.matrix(trainNorExp[uniqueFeatureGene,]))
#' y <- as.factor(trainResponseData$cellType)
#' classifier <- buildSVMclassifier(x, y)
#' 
#' # Save the classifier with feature genes
#' saveClassifier(classifier, file = "my_classifier.rds", 
#'               feature_genes = uniqueFeatureGene)
#' }
#'
#' @export
saveClassifier <- function(classifier, file, feature_genes = NULL, ...) {
  # Input validation
  if (!inherits(classifier, "svm")) {
    stop("The classifier must be an SVM model object")
  }
  
  if (!is.character(file) || length(file) != 1) {
    stop("file must be a single character string")
  }
  
  # Create a list containing all necessary components
  save_object <- list(
    model = classifier,
    feature_genes = feature_genes,
    timestamp = Sys.time(),
    version = packageVersion("cellAnnotator")
  )
  
  # Save the object
  tryCatch({
    saveRDS(save_object, file = file, ...)
    message(sprintf("Classifier successfully saved to %s", file))
    invisible(TRUE)
  }, error = function(e) {
    stop(sprintf("Failed to save classifier: %s", e$message))
  })
}

#' Load a saved classifier from a file
#'
#' @description
#' Loads a previously saved SVM classifier and its associated metadata from an RDS file.
#' The function performs version compatibility checks and returns the loaded classifier.
#' The loaded object includes:
#' \itemize{
#'   \item The SVM classifier model
#'   \item Feature genes used for classification (if available)
#'   \item Timestamp of when the classifier was saved
#'   \item Version of cellAnnotator used
#' }
#'
#' @param file Character string specifying the path to the saved classifier file
#' @param ... Additional arguments passed to \code{\link[base]{readRDS}}
#'
#' @return A list containing:
#' \itemize{
#'   \item model: The loaded SVM classifier
#'   \item feature_genes: The feature genes used for classification (if available)
#'   \item timestamp: When the classifier was saved
#'   \item version: Version of cellAnnotator used to save the classifier
#' }
#'
#' @examples
#' \dontrun{
#' # Load a previously saved classifier
#' saved_classifier <- loadClassifier("my_classifier.rds")
#' 
#' # Access the classifier model
#' classifier <- saved_classifier$model
#' 
#' # Access the feature genes
#' feature_genes <- saved_classifier$feature_genes
#' }
#'
#' @export
loadClassifier <- function(file, ...) {
  # Input validation
  if (!is.character(file) || length(file) != 1) {
    stop("file must be a single character string")
  }
  
  if (!file.exists(file)) {
    stop(sprintf("File not found: %s", file))
  }
  
  # Load the object
  tryCatch({
    loaded_object <- readRDS(file = file, ...)
    
    # Validate the loaded object structure
    required_components <- c("model", "timestamp", "version")
    if (!is.list(loaded_object) || !all(required_components %in% names(loaded_object))) {
      stop("Invalid classifier file format")
    }
    
    # Check if the model is an SVM object
    if (!inherits(loaded_object$model, "svm")) {
      stop("The loaded model is not an SVM object")
    }
    
    # Version compatibility check
    current_version <- packageVersion("cellAnnotator")
    saved_version <- loaded_object$version
    
    if (saved_version != current_version) {
      warning(sprintf(
        "The classifier was saved with cellAnnotator version %s, but you are using version %s. Compatibility is not guaranteed.",
        saved_version, current_version
      ))
    }
    
    message(sprintf(
      "Classifier loaded successfully (saved on %s with cellAnnotator version %s)",
      loaded_object$timestamp,
      saved_version
    ))
    
    if (!is.null(loaded_object$feature_genes)) {
      message(sprintf("Feature genes (%d) were also loaded", length(loaded_object$feature_genes)))
    }
    
    return(loaded_object)
    
  }, error = function(e) {
    stop(sprintf("Failed to load classifier: %s", e$message))
  })
} 