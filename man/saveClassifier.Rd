% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classifier_utils.R
\name{saveClassifier}
\alias{saveClassifier}
\title{Save a trained classifier to a file}
\usage{
saveClassifier(classifier, file, ...)
}
\arguments{
\item{classifier}{A trained SVM classifier object}

\item{file}{Character string specifying the path where the classifier should be saved}

\item{...}{Additional arguments passed to \code{\link[base]{saveRDS}}}
}
\value{
Invisibly returns TRUE if successful
}
\description{
Saves a trained SVM classifier along with its associated metadata to an RDS file.
The saved object includes the classifier model, feature genes, and normalization parameters.
}
\examples{
\dontrun{
# After training a classifier
saveClassifier(classifier, file = "my_classifier.rds")
}

}
