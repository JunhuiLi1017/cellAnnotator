#' filter single cell genes
#'
#' filter single cell genes based on the idea that reliably detected genes will either be detected in many cells, or highly expressed in a small cels of cells (or both)
#'
#' @param expDat single cell expression dataset
#'
#' @param mu the threshold of average expression of genes passing
#'
#' @param alpha1 the proportion of cells in which a gene must be considered detected
#'
#' @param alpha2 the proportion of cells for genes that have higher expression level
#'
#' @param threshold threshold of gene expression detection
#'
#' @param pseudo pseudo of calculation of the proportion of cells
#'
#' @examples
#'
#' data(alldataset)
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' filterGene <- filterSCGene(trainNorExp,mu = 2,alpha1 = 0.1,alpha2 = 0.01,threshold=0.25)
#'
#' @export

filterSCGene <- function(expDat,
                          mu = 2,
                          alpha1 = 0.1,
                          alpha2 = 0.01,
                          threshold=0,
                          pseudo=FALSE)
{
  ## calculate means of gene expression based on gene with expression > threshold
  meanGivenConds <- function(vector, threshold) {
    return(mean(vector[vector > threshold]))
  }

  muSet <- apply(expDat, 1, meanGivenConds, threshold)
  muSet[is.na(muSet)] <- 0

  ## calculate the ratio of samples with expression>threshold/total samples
  countGivenConds <- function(vector, threshold) {
    sum(vector > threshold)
  }
  countConds <- apply(expDat, 1, countGivenConds, threshold)
  ratioSet <- countConds/ncol(expDat)
  if (pseudo) {
    ratioSet <- (countConds + 1)/(ncol(expDat) + 1)
  }

  #find genes that pass criteria
  # based on idea that reliably detected genes will either be detected in many cells, or highly expressed in a small cels of cells (or both)
  pass1 <- names(which(ratioSet > alpha1))
  pass2 <- names(which((ratioSet <= alpha1 & ratioSet > alpha2) & muSet > mu))
  return(c(pass1,pass2))
}
