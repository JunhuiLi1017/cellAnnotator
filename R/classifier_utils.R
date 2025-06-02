#' Save a trained classifier to a file
#'
#' @description
#' Saves a trained SVM classifier along with its associated metadata to an RDS file.
#' The saved object includes the classifier model, feature genes, and normalization parameters.
#'
#' @param classifier A trained SVM classifier object
#' @param file Character string specifying the path where the classifier should be saved
#' @param ... Additional arguments passed to \code{\link[base]{saveRDS}}
#'
#' @return Invisibly returns TRUE if successful
#'
#' @examples
#' \dontrun{
#' # After training a classifier
#' saveClassifier(classifier, file = "my_classifier.rds")
#' }
#'
#' @export
saveClassifier <- function(classifier, file, ...) {
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
#'
#' @param file Character string specifying the path to the saved classifier file
#' @param ... Additional arguments passed to \code{\link[base]{readRDS}}
#'
#' @return The loaded classifier object
#'
#' @examples
#' \dontrun{
#' # Load a previously saved classifier
#' classifier <- loadClassifier("my_classifier.rds")
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
    if (!is.list(loaded_object) || !all(c("model", "timestamp", "version") %in% names(loaded_object))) {
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
    
    return(loaded_object$model)
    
  }, error = function(e) {
    stop(sprintf("Failed to load classifier: %s", e$message))
  })
} 