% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotROC.R
\name{plotROC}
\alias{plotROC}
\title{plot ROC curve}
\usage{
plotROC(evalMetrics, collapse = FALSE)
}
\arguments{
\item{evalMetrics}{output of evaluateClassifier function}

\item{collapse}{collapse all roc curve or not, defaul is FALSE}
}
\description{
plot ROC curve for each classifier
}
\examples{

data(evalMetrics)
\dontrun{
plotROC(evalMetrics)
}

}
