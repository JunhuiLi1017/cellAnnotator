#' normalize single cell expression dataset
#'
#' normalize single cell expression dataset
#'
#' @param expDat single cell expression dataset
#'
#' @param total explanatory dataset
#'
#' @param dThresh numeric or character variable indicating the response variable in response dataset
#'
#' @examples
#'
#' data(alldataset)
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' testNorExp <- normalizeSCExp(alldataset$testExplanatoryData,dThresh = 0.25)
#'
#' @import caTools
#'
#' @importFrom Matrix colSums t
#'
#' @export

normalizeSCExp <- function (expDat, total=10000, dThresh = 0) {
  if (class(expDat)[1] != "matrix") {
    cSums <- Matrix::colSums(expDat)
    props <- Matrix::t(expDat)/cSums
    rrids <- cSums - total
    tmpAns <- expDat - Matrix::t(props * rrids)
    tmpAns[Matrix::which(tmpAns < dThresh)] <- 0
  } else {
    cSums <- colSums(expDat)
    props <- t(expDat)/cSums
    rrids <- cSums - total
    tmpAns <- expDat - t(props * rrids)
    tmpAns[which(tmpAns < dThresh)] <- 0
  }
  normDat = log(1 + tmpAns)
  return(normDat)
}
