#' heat map plot
#'
#' make heatmap plot between predicted and actual categories
#'
#' @param predY Predicted response variable
#'
#' @param queryY A vector of response variable named with independent variables
#'
#' @examples
#'
#' data(alldataset)
#' data(uniqueFeatureGene)
#' queryY <- alldataset$testResponseData$cellType
#' names(queryY) <- alldataset$testResponseData$cell
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
#' y=as.factor(alldataset$trainResponseData$cellType)
#' svmClassifier <- buildSVMclassifier(x,y)
#' testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
#' testX=t(as.matrix(testNorExp[uniqueFeatureGene,]))
#' predY <- predictWithclassifier(svmClassifier,testX)
#' \dontrun{
#' plotAttr(predY,queryY)
#' }
#'
#' @import ggplot2 scales
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export

plotAttr <- function(predY,queryY){
  df <- cbind(data.frame(queryY),data.frame(predY))
  colnames(df) <- c("actual_value","predicted_value")
  pal = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(predY)))

  p = ggplot(df, aes_string(x = "actual_value", fill = "predicted_value")) + geom_bar(position = "fill", width = 0.6) + scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = pal) + theme_bw() + coord_flip()
  return(p)
}
