#' Find Feature Genes for Cell Type Classification
#'
#' @description
#' Identifies the most informative genes for cell type classification using a least squares approach.
#' For each cell type, the function:
#' \itemize{
#'   \item Creates a binary response vector (1 for cells of the target type, 0 for others)
#'   \item Fits a least squares model for each gene
#'   \item Calculates correlation coefficients and p-values
#'   \item Selects top genes based on correlation values
#' }
#'
#' @param responseData A character or factor vector containing cell type labels for each cell
#' @param explanatoryData A matrix or sparse matrix of gene expression data (genes in columns, cells in rows)
#' @param filteredGene A character vector of genes that have passed initial filtering criteria
#' @param topX Integer specifying the number of top genes to select for each cell type
#' @param reverse Logical. If TRUE (default), selects both top and bottom X genes based on correlation values
#'
#' @return A character vector of unique feature genes selected for classification
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Creates binary response vectors for each cell type
#'   \item Fits least squares models for each gene
#'   \item Calculates correlation coefficients and p-values
#'   \item Adjusts p-values using Holm's method
#'   \item Selects top X genes based on correlation values
#'   \item If reverse=TRUE, also selects bottom X genes
#'   \item Returns unique set of selected genes
#' }
#'
#' @examples
#' # Load example data
#' data(alldataset)
#' 
#' # Normalize expression data
#' trainNorExp <- normalizeSCExp(
#'   alldataset$trainExplanatoryData,
#'   dThresh = 0.25
#' )
#' 
#' # Filter genes
#' filterGene <- filterSCGene(
#'   trainNorExp,
#'   mu = 2,
#'   alpha1 = 0.1,
#'   alpha2 = 0.01,
#'   threshold = 0.25
#' )
#' 
#' # Get cell type labels
#' responseData <- alldataset$trainResponseData$cellType
#' 
#' # Find feature genes
#' uniqueFeatureGene <- findFeatureGene(
#'   responseData,
#'   trainNorExp,
#'   filteredGene = filterGene,
#'   topX = 20,
#'   reverse = TRUE
#' )
#'
#' @importFrom Matrix t
#' @importFrom stats ls.print lsfit p.adjust
#' @importFrom utils head tail
#'
#' @export
findFeatureGene <- function(responseData,
                            explanatoryData,
                            filteredGene,
                            topX = 50,
                            reverse=TRUE){
  # Input validation
  if (!is.vector(responseData)) {
    stop("responseData must be a vector")
  }
  
  if (!is.matrix(explanatoryData) && !inherits(explanatoryData, "Matrix")) {
    stop("explanatoryData must be a matrix or sparse matrix")
  }
  
  if (!is.character(filteredGene)) {
    stop("filteredGene must be a character vector")
  }
  
  if (!is.numeric(topX) || length(topX) != 1 || topX <= 0) {
    stop("topX must be a positive integer")
  }
  
  if (!is.logical(reverse) || length(reverse) != 1) {
    stop("reverse must be a single logical value")
  }

  # Create binary response vectors for each cell type
  binaryYList <- list()
  for (i in unique(as.vector(responseData))) {
    x <- rep(0, length(responseData))
    x[responseData == i] <- 1
    binaryYList[i] <- list(x)
  }

  # Function to fit least squares model and calculate statistics
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
    data.frame(row.names = geneids, pval = pval, cval = cval, holm = holm)
  }

  # Transpose data if needed
  if(class(explanatoryData)[1]!='matrix'){
    explanatoryData = Matrix::t(explanatoryData)
  }else{
    explanatoryData = t(explanatoryData)
  }

  # Fit models for each cell type
  featureGeneList <- lapply(binaryYList, lsFit, dat=explanatoryData)

  # Function to select top X genes
  getFeatureGeneTopX <- function(subElement, topX=25, reverse=reverse){
    subElement <- subElement[!is.na(subElement$cval),]
    orderSubElement <- subElement[order(subElement$cval, decreasing=TRUE),]
    fGeneTopX <- rownames(utils::head(orderSubElement, topX))

    if(reverse){
      fGeneTopX <- append(fGeneTopX, rownames(utils::tail(orderSubElement, topX)))
    }
    return(fGeneTopX)
  }

  # Select top genes for each cell type
  featureGeneTopXList <- lapply(featureGeneList, getFeatureGeneTopX, topX=topX, reverse=reverse)

  # Return unique set of selected genes
  uniqFeatureGene <- unique(unlist(featureGeneTopXList))
  return(uniqFeatureGene)
}
