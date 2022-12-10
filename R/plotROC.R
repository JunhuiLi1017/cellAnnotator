#' plot ROC curve
#'
#' plot ROC curve for each classifier
#'
#' @param evalMetrics output of evaluateClassifier function
#'
#' @param collapse collapse all roc curve or not, defaul is FALSE
#'
#' @examples
#'
#' data(evalMetrics)
#' \dontrun{
#' plotROC(evalMetrics)
#' }
#'
#' @import ggplot2 pROC hrbrthemes
#'
#' @export

plotROC <- function(evalMetrics,collapse=FALSE){
  rocObjList <- evalMetrics[["rocObjList"]]
  g <- ggroc(rocObjList)+ theme(panel.grid.minor = element_blank())
  if(collapse==FALSE){
    #g + facet_grid(.~name) + theme(legend.position="none")
    g + facet_wrap( ~ name) + theme(legend.position="none")
  }else{
    g
  }
}
