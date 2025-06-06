% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotHeatmap.R
\name{plotHeatmap}
\alias{plotHeatmap}
\title{heat map plot}
\usage{
plotHeatmap(predY, queryY, maxSubClass = 5000)
}
\arguments{
\item{predY}{output of predict function}

\item{queryY}{A vector of response variable named with independent variables}

\item{maxSubClass}{the max number of cell in each class are allowed}
}
\description{
make heatmap plot between predicted and actual categories
}
\examples{

data(alldataset)
data(uniqueFeatureGene)
testY <- alldataset$testResponseData$cellType
names(testY) <- alldataset$testResponseData$cell
trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
y=as.factor(alldataset$trainResponseData$cellType)
svmClassifier <- buildSVMclassifier(x,y)
testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
testX=t(as.matrix(testNorExp[uniqueFeatureGene,]))
predY <- predictWithclassifier(svmClassifier,testX)
\dontrun{
plotHeatmap(predY,testY)
}

}
