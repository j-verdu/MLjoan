# ----------------------------------------------------------------------
# Simple 3 classes discriminant function
# ----------------------------------------------------------------------
#' discriminant3 function 
#' 
#' Performs linear discriminant classification using 3 categories.
#'
#'
#' @param data A data frame where rows are observations and 
#' columns are features, labeled with a name. 
#' Observations must be ordered by categories. First n1 observations belong to 
#' first category; then n2 observations to secornd category and n3 observations
#' to third category.
#' @param number_each A numeric vector with number of observations per category: 
#' n1, n2, n3, following same order as \code{data}.
#' @param names_each A character vector with the labels of each category, 
#' following same order as \code{data} and \code{number_each}.
#' @return A list with following elements: predicted labels, performance (in 
#' total counts) and performanceProp (in proportion) 
#' @export
#' @import assertthat mvtnorm
#' @examples
#' # First create artificial dataset of Foxs, Cats and Dogs
#' 
#' # create covariance function
#' sigmaXY <- function(rho, sdX, sdY) {
#'     covTerm <- rho * sdX * sdY
#'     VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
#'                        2, 2, byrow = TRUE)
#'     return(VCmatrix)
#' }
#' 
#' # Generate bivariate normal data
#' library(mvtnorm)
#' genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
#'     if(!is.na(seed)) set.seed(seed)
#'     rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
#'     return(rdraws)
#' }
#' 
#' # general function
#' gen_data <- function(noFoxs,noCats, noDogs, muFoxs,muCats, muDogs, sdFoxs,sdCats, 
#'                             sdDogs, rhoFoxs,rhoCats, rhoDogs, seed=1111) {
#'     sigmaFoxs <- sigmaXY(rho=rhoFoxs, sdX=sdFoxs[1], sdY=sdFoxs[2])
#'     sigmaCats <- sigmaXY(rho=rhoCats, sdX=sdCats[1], sdY=sdCats[2])
#'     sigmaDogs <- sigmaXY(rho=rhoDogs, sdX=sdDogs[1], sdY=sdDogs[2])
#'     foxs<- genBVN(noFoxs, muFoxs, sigmaFoxs, seed = seed)
#'     cats <- genBVN(noCats, muCats, sigmaCats, seed = seed+1)
#'     dogs <- genBVN(noDogs, muDogs, sigmaDogs, seed = seed+2)
#'     animalsDf <- as.data.frame(rbind(foxs,cats,dogs))
#'     colnames(animalsDf) <- c("weight", "height")
#'     return(animalsDf)
#' }
#'
#' # Parameters to generate data
#' noFoxs<- noCats <- noDogs <- 50
#' muFoxs<- c(6,170)
#' muCats <- c(4, 150)
#' muDogs <- c(8, 100)
#' sdFoxs<-c(1.5,20)
#' sdCats<-c(1,20)
#' sdDogs<- c(2,30)
#' rhoFoxs<-0.1
#' rhoCats<--0.1
#' rhoDogs<-0.6
#'
#' #Generate data
#' animalsDf <- gen_data(noFoxs,noCats, noDogs, muFoxs,muCats, muDogs, 
#'                             sdFoxs,sdCats, sdDogs, rhoFoxs,rhoCats, rhoDogs)
#'
#' # Apply discriminant function
#' classes<-c("Foxs","Cats","Dogs")
#' out<- discriminant3(animalsDf,c(noFoxs,noCats,noDogs),classes)
#' out$performance
#'  


discriminant3<-function(data,number_each,names_each){
    
    not_empty(data); not_empty(number_each); not_empty(names_each)
    assert_that(nrow(data)==sum(number_each))
    assert_that(length(names_each)==length(number_each))
    
    noFoxs<-number_each[1]
    noCats<-number_each[2]
    noDogs<-number_each[3]
    names_var<-names(data)
    labels<-c(rep(names_each[1],noFoxs),
              rep(names_each[2],noCats),
              rep(names_each[3],noDogs)
              )
    
    # Predict probabilities per category
    
    # analytical solution
    X <- as.matrix(cbind(ind=rep(1, nrow(data)), 
                         data[,names_var]))
    Y <- cbind(targetFoxs = c(rep(1, noFoxs),rep(0, noCats), rep(0, noDogs)),
               targetCats = c(rep(0, noFoxs),rep(1, noCats), rep(0, noDogs)),
               targetDogs = c(rep(0, noFoxs),rep(0, noCats), rep(1, noDogs))
    ) 
    weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y
    
    # compute predictions
    predictions <- X %*% weightsOptim
    
    # classify according to the argmax criterion
    predictedLabels <- rep(names_each[2], nrow(data))
    predictedLabels[(predictions==apply(predictions, 1, max))[,1]] <- names_each[1]
    predictedLabels[(predictions==apply(predictions, 1, max))[,3]] <- names_each[3]
    
    # classification algorithm performance
    performance <- table(labels, predictedLabels)
    performanceProp <- prop.table(performance, 1)
    
    return(list(predictedLabels=predictedLabels,
                performance=performance,performanceProp=performanceProp))

}

