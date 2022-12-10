#' find feature gene
#'
#' find feature gene based on least square method
#'
#' @param responseData response vector
#'
#' @param explanatoryData explanatory dataset
#'
#' @param filteredGene output of filterSCGene
#'
#' @param topX a numeric vector to select the top X genes
#'
#' @param reverse select the bottom X genes, logistic value, defaul is TRUE.
#'
#' @examples
#'
#' data(alldataset)
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' filterGene <- filterSCGene(trainNorExp,mu = 2,alpha1 = 0.1,alpha2 = 0.01,threshold=0.25)
#' responseData <- alldataset$trainResponseData$cellType
#' uniqueFeatureGene <- findFeatureGene(responseData,trainNorExp,filteredGene=filterGene,
#' topX = 20,reverse=TRUE)
#'
#' @importFrom Matrix t
#'
#' @importFrom stats ls.print lsfit p.adjust
#'
#' @importFrom utils head tail
#'
#' @export

findFeatureGene <- function(responseData,
                            explanatoryData,
                            filteredGene,
                            topX = 50,
                            reverse=TRUE){
  binaryYList <- list()
  for (i in unique(as.vector(responseData))) {
    x <- rep(0, length(responseData))
    x[responseData == i] <- 1
    binaryYList[i] <- list(x)
  }

  lsFit <- function (pattern, dat){
    pval <- vector()
    cval <- vector()
    geneids <- colnames(dat)

    lsfitResult <- ls.print(lsfit(pattern, dat), digits = 25, print.it = FALSE)
    fitMat <- matrix(unlist(lsfitResult$coef), ncol = 8, byrow = TRUE)
    ccorr <- fitMat[, 6]
    cval <- sqrt(as.numeric(lsfitResult$summary[, 2])) * sign(ccorr)
    pval <- as.numeric(fitMat[, 8])
    holm <- p.adjust(pval, method = "holm")
    data.frame(row.names = geneids, pval = pval, cval = cval,holm = holm)
  }
  if(class(explanatoryData)[1]!='matrix'){
    explanatoryData = Matrix::t(explanatoryData)
  }else{
    explanatoryData = t(explanatoryData)
  }
  featureGeneList <- lapply(binaryYList, lsFit, dat=explanatoryData)

  ## extract top X feature gene based on the sort value of C value
  getFeatureGeneTopX <- function(subElement,topX=25, reverse=reverse){
    subElement <- subElement[!is.na(subElement$cval),]
    orderSubElement <- subElement[order(subElement$cval, decreasing=TRUE),]
    fGeneTopX <- rownames(utils::head(orderSubElement,topX))

    if(reverse){
      fGeneTopX <- append(fGeneTopX,rownames(utils::tail(orderSubElement,topX)))
    }
    return(fGeneTopX)
  }

  featureGeneTopXList <- lapply(featureGeneList,getFeatureGeneTopX,topX=topX,reverse=reverse)

  uniqFeatureGene <- unique(unlist(featureGeneTopXList))
  return(uniqFeatureGene)
}
