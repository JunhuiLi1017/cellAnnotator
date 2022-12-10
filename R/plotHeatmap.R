#' heat map plot
#'
#' make heatmap plot between predicted and actual categories
#'
#' @param predY output of predict function
#'
#' @param queryY A vector of response variable named with independent variables
#'
#' @param maxSubClass the max number of cell in each class are allowed
#'
#' @examples
#'
#' data(alldataset)
#' data(uniqueFeatureGene)
#' testY <- alldataset$testResponseData$cellType
#' names(testY) <- alldataset$testResponseData$cell
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
#' y=as.factor(alldataset$trainResponseData$cellType)
#' svmClassifier <- buildSVMclassifier(x,y)
#' testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
#' testX=t(as.matrix(testNorExp[uniqueFeatureGene,]))
#' predY <- predictWithclassifier(svmClassifier,testX)
#' \dontrun{
#' plotHeatmap(predY,testY)
#' }
#'
#' @import RColorBrewer pheatmap
#'
#' @export

plotHeatmap <- function(predY, queryY, maxSubClass=5000){
  backgroundCols <- colorRampPalette(c("black", "limegreen", "yellow"))(100)
  #backgroundCols <- heat.colors(100)
  #queryY <- queryY[order(queryY)]
  classNames <- sort(unique(queryY))
  sampledCells <- NULL
  allCT <- NULL
  #i=classNames[1]
  for (i in classNames) {
    subqueryY <- names(queryY)[queryY == i]
    if (length(subqueryY) > maxSubClass) {
      eachCell <- sample(subqueryY, maxSubClass)
    }else {
      eachCell <- subqueryY
    }
    eachCT <- as.factor(rep(i,length(eachCell)))
    names(eachCT) <- eachCell
    allCT <- append(allCT,eachCT)
    sampledCells <- append(sampledCells, eachCell)
  }

  probMatrix <- t(attr(predY,"probabilities"))
  subProMat <- probMatrix[,sampledCells]

  xcol <- colorRampPalette(rev(brewer.pal(n = 12, name = "Paired")))(length(classNames))
  names(xcol) <- classNames
  anno_colors <- list(group = xcol)
  xx <- data.frame(group = allCT)

  pheatmap(subProMat, color = backgroundCols,
           breaks = seq(from = 0, to = 1, length.out = 100),
           cluster_rows = FALSE, cluster_cols = FALSE,
           show_colnames = FALSE, annotation_names_row = FALSE,
           annotation_col = xx, annotation_names_col = FALSE,
           annotation_colors = anno_colors)

}
