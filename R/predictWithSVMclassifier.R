#' predict classification with Support Vector Machines classifier
#'
#' predicts classification based on a model trained by svm function
#'
#' @param classifier classifier outputted from buildSVMclassifier function
#'
#' @param x explanatory data set, note that the order of variable names should be the same as training dataset
#'
#' @examples
#'
#' data(uniqueFeatureGene)
#' data(alldataset)
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
#' y=as.factor(alldataset$trainResponseData$cellType)
#' classifier <- buildSVMclassifier(x,y)
#' testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
#' testX=t(as.matrix(testNorExp[uniqueFeatureGene,]))
#' predTestY <- predictWithclassifier(classifier,testX)
#'
#' @import e1071
#'
#' @importFrom stats predict
#'
#' @export

predictWithclassifier <- function(classifier,x){
  predY = predict(object=classifier, newdata = x,probability = TRUE)
  return(predY)
}

