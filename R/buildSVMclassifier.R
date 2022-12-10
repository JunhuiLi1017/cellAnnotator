#' build SVM classifier
#'
#' build SVM classifier as a classification machine
#'
#' @param x explanatory data set.
#'
#' @param y a response vector with one label for each row/component of x. And this should be a factor vector.
#'
#' @param scale A logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed. Per default, data are scaled internally (both x and y variables) to zero mean and unit variance. The center and scale values are returned and used for later predictions.
#'
#' @param type svm can be used as a classification machine in this function. The default setting for type is C-classification.Valid options are:
#' C-classification
#'
#' @param kernel the kernel used in training and predicting. see kernel of svm for detail message.
#'
#' @examples
#'
#' data(uniqueFeatureGene)
#' data(alldataset)
#' trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData,dThresh = 0.25)
#' x=t(as.matrix(trainNorExp[uniqueFeatureGene,]))
#' y=as.factor(alldataset$trainResponseData$cellType)
#' svmClassifier <- buildSVMclassifier(x,y)
#'
#' @import e1071
#'
#' @export

buildSVMclassifier <- function(x,
                               y,
                               scale = TRUE,
                               type=c('C-classification'),
                               kernel = c('radial','linear','polynomial','sigmoid')
                               ){
  type <- match.arg(type)
  kernel <- match.arg(kernel)
  classifier <- e1071::svm(x=x,
                    y=y,
                    scale=scale,
                    type = type,
                    kernel = kernel,
                    probability = TRUE)
  return(classifier)
}
