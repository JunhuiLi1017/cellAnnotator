#' split dataset into train and test dataset
#'
#' split dataset into train and test dataset based on ratio of the number of each response variable or the given number
#'
#' @param responseData response dataset
#'
#' @param explanatoryData explanatory dataset
#'
#' @param groupBy numeric or character variable indicating the response variable in response dataset
#'
#' @param sampleID numeric or character variable indicating the sample id in response dataset
#'
#' @param splitParameter which parameter to be used to split dataset
#'
#' @param splitRatio ratio to split the dataset into train dataset
#'
#' @param splitNum number of samples to split the dataset into train dataset, if the total sample number is less than splitNum, use splitRatio instead.
#'
#' @param seed seed number for split
#'
#' @examples
#'
#' data(reference_meta)
#' data(reference_expr)
#' datalist <- splitTrainTest(reference_meta,reference_expr,
#' groupBy="cellType",sampleID="cell",splitParameter="ratio")
#'
#' @import caTools
#'
#' @export

splitTrainTest <- function(responseData,explanatoryData,groupBy,sampleID,splitParameter=c("ratio","number"),splitRatio=0.6,splitNum=100,seed=1234){
  ## Split for training and assessment, and transform training data
  set.seed(seed) #can be any random seed number
  indexTrain <- NULL
  allResponse <- responseData[,groupBy]
  if(splitParameter=="ratio"){
    #i=unique(allResponse)[2]
    for(i in unique(allResponse)){
      indexTrain <- append(indexTrain,sample.split(allResponse[allResponse %in% i],SplitRatio=splitRatio))
    }
    indexTrain <- which(indexTrain)
  }else if(splitParameter=="number"){
    for(i in unique(allResponse)){
      subSum <- sum(allResponse %in% i)
      if(subSum < splitNum){
        subSplitNum <- round(subSum*splitRatio)
      }else{
        subSplitNum <- splitNum
      }
      indexTrain <- append(indexTrain,sort(sample(which(allResponse %in% i),size=subSplitNum)))
    }
  }

  indexTest <- setdiff(1:nrow(responseData),indexTrain)
  trainID <- responseData[indexTrain,sampleID]
  testID <- responseData[indexTest,sampleID]

  trainResData <- responseData[indexTrain,]
  testResData <- responseData[indexTest,]
  trainExpData <- explanatoryData[,colnames(explanatoryData) %in% trainID]
  testExpData <- explanatoryData[,colnames(explanatoryData) %in% testID]

  resultDataset <- list()
  resultDataset["trainResponseData"] <- list(trainResData)
  resultDataset["testResponseData"] <- list(testResData)
  resultDataset["trainExplanatoryData"] <- list(trainExpData)
  resultDataset["testExplanatoryData"] <- list(testExpData)
  return(resultDataset)
}
