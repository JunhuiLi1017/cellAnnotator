% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictWithSVMclassifier.R
\name{predictWithclassifier}
\alias{predictWithclassifier}
\title{predict classification with Support Vector Machines classifier}
\usage{
predictWithclassifier(classifier, x)
}
\arguments{
\item{classifier}{classifier outputted from buildSVMclassifier function}

\item{x}{explanatory data set, note that the order of variable names should be the same as training dataset}
}
\description{
predicts classification based on a model trained by svm function
}
\examples{

data(uniqueFeatureGene)
data(alldataset)
trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
y=as.factor(alldataset$trainResponseData$cellType)
classifier <- buildSVMclassifier(x,y)
testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
testX=t(as.matrix(testNorExp[uniqueFeatureGene,]))
predTestY <- predictWithclassifier(classifier,testX)

}
