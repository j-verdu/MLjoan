# ----------------------------------------------------------------------
# Simple kNN classifier
# ----------------------------------------------------------------------
#' kNN classifier 
#' 
#' Classify the input with a k nearest neighbors classifier.
#'
#' @param data A data frame or a matrix where rows are observations and 
#' columns are features. If \code{type} is "train" this is training 
#' dataset, and if it is "predict" it is test dataset.
#' @param trueClasses A vector with labels for each row in \code{data} 
#' if \code{type} is "train", and with labels for each row in 
#' \code{memory} if \code{type} is "predict".
#' @param neighbors A data frame containing for each data point of \code{data}, 
#' the ordered list of nearests neighbors (each column is a vector of indexes 
#' pointing to nearest neighbors).
#' @param memory A data frame or a matrix where rows are observations 
#' and columns are features. If \code{type} is "train" this argument 
#' is not needed, and if it is "predict" it is a training dataset.
#' @param k Number of neighbors that the classifier should use. It has 
#' to be an odd number. 
#' @param type Whether the goal is to train the classifier or predict 
#' classes of new observations based on past ones. The value can be 
#' either "train" or "predict".
#' @return A list with following elements: predictedClasses, prob, 
#' accuracy and errorCount.
#' @export
#' @import assertthat 
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(200), ncol=2)
#' inputsTrain  <- matrix(rnorm(200), ncol=2)
#' classesTrain <- c(rep(0, 50), rep(1, 50))
#' # compute distances and neighbors
#' distMatrix<-distance(inputsTest, memory=inputsTrain,p=2,type="predict")
#' neighbors<-calculate_neighbors(distMatrix)
#' # get the kNN predictions for the test set
#' out<-kNN_classifier(inputsTest, classesTrain, neighbors, inputsTrain, 
#' k=15, type="predict")
#' out$predictedClasses
#' 



kNN_classifier <- function(data, trueClasses, neighbors, memory=NULL, 
                           k=1,  type="train") {
    # test the inputs
    
    not_empty(data); not_empty(trueClasses); not_empty(neighbors)
    if (type=="train") {
        assert_that(nrow(data)==length(trueClasses))
        assert_that(nrow(data)==ncol(neighbors))
    }
    is.string(type); assert_that(type %in% c("train", "predict"))
    is.count(k); 
    if (type=="predict") {
        assert_that(not_empty(memory) & 
                        ncol(memory)==ncol(data) & 
                        nrow(memory)==length(trueClasses))
        assert_that(nrow(memory)==nrow(neighbors))
    }
    
    # Compute the distance between each point and all others 
    noObs <- nrow(data)
    
    if (type=="train") {
        predictionId <- 1
        
    } else if (type == "predict") {
        predictionId <- 0
    }
    
    # Compute and return the most frequent class in the k nearest neighbors
    prob<- predictedClasses <-  rep(NA, noObs)
    for (obs in 1:noObs) {
        kNN<-trueClasses[neighbors[(1+predictionId):
                                       (k+predictionId), obs]]
        
        kNNclasses<-sort(table(kNN),TRUE)
        predictedClasses[obs] <- names(kNNclasses)[1]
        prob[obs]<-kNNclasses[1]/k
    }
    
    # examine the performance, available only if training
    if (type=="train") {
        errorCount <- table(predictedClasses, trueClasses)
        accuracy <- mean(predictedClasses==trueClasses)
    } else if (type == "predict") {
        errorCount <- NA
        accuracy <- NA
    }
    
    # return the results
    return(list(predictedClasses=predictedClasses, 
                prob=prob,
                accuracy=accuracy,
                errorCount=errorCount))
}


