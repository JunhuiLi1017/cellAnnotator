#' evaluate the classifier
#'
#' Computing Classification Evaluation Metrics
#'
#' @param predY output of predict result from predict function
#'
#' @param queryY A vector of response variable named with independent variables
#'
#' @examples
#'
#' data(uniqueFeatureGene)
#' data(alldataset)
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
#' y=as.factor(alldataset$trainResponseData$cellType)
#' svmClassifier <- buildSVMclassifier(x,y)
#' testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
#' testX=t(as.matrix(testNorExp[uniqueFeatureGene,]))
#' yPred <- predictWithclassifier(svmClassifier,testX)
#' yActual <- as.factor(alldataset$testResponseData$cellType)
#' names(yActual) <- alldataset$testResponseData$cell
#' evaluatedResult <- evaluateClassifier(yPred,yActual)
#'
#' @importFrom MLmetrics MultiLogLoss
#'
#' @importFrom caret confusionMatrix
#'
#' @export

evaluateClassifier <- function(predY,
                        queryY){
  #subsetting the probMatrix where the cells' true identity is within the range of the classifiers
  probMatrix <- attr(predY, "probabilities")
  #colnames(probMatrix) <- unique(queryY)
  result <- list()
  # multiple log loss
  multiLogLoss <- MLmetrics::MultiLogLoss(y_true = queryY, y_pred = probMatrix)
  #confusionMatrix
  cfMatrix <- caret::confusionMatrix(data=predY, reference = queryY)
  probMatrix11 <- cbind(as.data.frame(as.character(queryY)),probMatrix)
  colnames(probMatrix11)[1] <- c("cellLable")
  probMatrix12 <- probMatrix11
  rocObjList <- list()
  for(k in 2:ncol(probMatrix11)){
    nActPos <- which(probMatrix11[,1] %in% colnames(probMatrix11)[k])
    nActNeg <- setdiff(c(1:nrow(probMatrix11)),nActPos)
    probMatrix12[nActPos,1] <- colnames(probMatrix11)[k]
    probMatrix12[nActNeg,1] <- "others"

    rocObj <- roc(probMatrix12[,1], probMatrix12[,k])
    rocObjList[colnames(probMatrix11)[k]] <- list(rocObj)
    #plot.roc(pp1,print.auc=TRUE,legacy.axes = TRUE, print.auc.y=0.8, print.auc.x=-0.05, grid=TRUE, auc.polygon=TRUE)
  }

  result[['multiLogLoss']] <- multiLogLoss
  result[['confusionMatrix']] <- cfMatrix
  result[['rocObjList']] <- rocObjList
  return(result)
}


