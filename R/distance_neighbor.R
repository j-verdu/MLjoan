# ----------------------------------------------------------------------
# Simple distance function
# ----------------------------------------------------------------------
#' distance function 
#' 
#' Calculate the distances between different input data.
#'
#'
#' @param data A data frame or a matrix where rows are observations and 
#' columns are features. If \code{type} is "train" this is training 
#' dataset, and if it is "predict" it is test dataset.
#' @param memory A data frame or a matrix where rows are observations 
#' and columns are features. If \code{type} is "train" this argument 
#' is not needed, and if it is "predict" it is a training dataset.
#' @param p Distance metric the classifier should use, the value can be 
#' either 1, 2 or Inf.
#' @param type Whether the goal is to train the classifier or predict 
#' classes of new observations based on past ones. The value can be 
#' either "train" or "predict".
#' @return If \code{type} is "train", a squared symmetric matrix whose i,j value is the is the distance between
#' data points i and j. Note that i,j are data observations related to same rows in 
#' \code{data} file. If \code{type} is "predict", a matrix with as many rows as 
#' observations in \code{data} and as many columns as observations in 
#' \code{memory}; value i,j is distance between data[i] and memory[j].
#' @export
#' @import assertthat 
#' @examples
#  create artificial dataset
#' inputsTest   <- matrix(rnorm(200), ncol=2)
#' inputsTrain  <- matrix(rnorm(200), ncol=2)
#' # compute distances
#' distMatrix<-distance(inputsTest, memory=inputsTrain,p=2,type="predict")
#' 
#' 


distance<- function(data, memory=NULL,p=2,type="train"){
    # if memory null, calculate distance between 'data' points
    # otherwise, distance from data(test) to 'memory'(train) data
    
    
    # test the inputs
    
    not_empty(data)
    is.string(type); assert_that(type %in% c("train", "predict")) 
    assert_that(p %in% c(1, 2, Inf))
    if (type=="predict") {
        assert_that(not_empty(memory) & 
                        ncol(memory)==ncol(data))
    }
    
    # Compute the distance between each point and all others 
    noObs <- nrow(data)
    
    # if we are making predictions on the test set based on the memory, 
    # we compute distances between each test observation and observations
    # in our memory
    if (type=="train") {
        distMatrix <- matrix(0, noObs, noObs)
        for (obs in 1:(noObs-1)) {
            
            # getting the probe for the current observation
            # avoiding to recompute computed distances
            probe <- as.numeric(data[obs,])
            probeExpanded <- matrix(rep(probe, each=(noObs-obs)), nrow=noObs-obs)
            
            # computing distances between the probe and exemplars in the
            # training data
            if (p %in% c(1,2)) {
                distMatrix[obs, (obs+1):noObs] <- (rowSums((abs(data[(obs+1):noObs,] - 
                                                                    probeExpanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, (obs+1):noObs] <- apply(abs(data[(obs+1):noObs,] - 
                                                                probeExpanded), 1, max)
            }  
        }
        # now we have lower triangular matrix, build final matrix (diagonals are 0)
        distMatrix<- distMatrix + t(distMatrix)
        
    } else if (type == "predict") {
        noMemory <- nrow(memory)
        distMatrix <- matrix(NA, noObs, noMemory)
        for (obs in 1:noObs) {
            
            # getting the probe for the current observation
            probe <- as.numeric(data[obs,])
            probeExpanded <- matrix(rep(probe, each=noMemory), nrow=noMemory)
            
            # computing distances between the probe and exemplars in the memory
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(memory - 
                                                       probeExpanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
            }  
        }
    }
    
    return(distMatrix)
    
}

# ----------------------------------------------------------------------
# Simple neighbor function
# ----------------------------------------------------------------------
#' calculate_neighbors function 
#' 
#' For each point, sort the other points according to a distance matrix to find 
#' a sortest list of points ordered by distance (nearest neighbors).
#'
#'
#' @param distMatrix A distance matrix between data points.
#' @return A matrix which includes, for each data point, column index to the
#'  ordered nearest neighbors. Note that first row is just de data point index
#'  into consideration.
#' @export
#' @import assertthat 
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(200), ncol=2)
#' inputsTrain  <- matrix(rnorm(200), ncol=2)
#' # compute distances 
#' distMatrix<-distance(inputsTest, memory=inputsTrain,p=2,type="predict")
#' # compute neighbors
#' neighbors<-calculate_neighbors(distMatrix)
#' 


calculate_neighbors<- function(distMatrix){
    # Sort the distances in increasing numerical order and pick the first 
    # k elements
    # test the inputs
    
    not_empty(distMatrix)
    
    neighbors <- apply(distMatrix, 1, order)
    return (as.data.frame(neighbors))
}
