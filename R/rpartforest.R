#' Build a random forest with base learners grown from rpart
#'
#' @description
#' This is a rudimentary implementation of Random Forest using rpart.
#' It primarily serves as a proof-of-concept to utilize the "treelm" function
#' to enable the representation of a Random Forest as a collection of design matrices
#'
#' @param y - outcome variable
#' @param x - explanatory covariates
#' @param test - testing data
#' @param mtry - mtry tuning parameter
#' @param ntrees - number of trees within the forest
#' @param getlm - boolean to indicate whether linear model representation should be computed
#' @param contrl- control parameters for rpart; refer to that documentation for details
#'
#' @return A matrix of predicted values from each base learner as well as
#' training and testing ANOVA matrices
#'
#' @author Ian Frankenburg
#'
#' @export
rpartforest <- function(y, x, test, mtry, ntrees, getlm = F,
                        contrl=rpart.control(minsplit = 5,
                                             minbucket = 1,
                                             cp = 0.001)){
  n <- nrow(x); p <- ncol(x); mtry=p
  predictions <- matrix(0, nrow=nrow(test), ncol=ntrees)
  indices <- matrix(0, nrow=n, ncol=ntrees)
  xlist <- testdata <- list()
  for(i in 1:ntrees){
    bootstrap_sample <- sample(1:n, size = n, replace = T)
    mtry_covariates <- sample(1:p, mtry, replace=F)
    x_ <- x[bootstrap_sample, mtry_covariates]
    y_ <- y[bootstrap_sample]
    # grow single decision tree
    fit <- rpart(y_ ~., data=cbind(y_,x_), control=contrl)
    predictions[, i] <- rpart.predict(fit, newdata = test)
    if(getlm){
      # convert tree to ANOVA model
      anova_tree <- treelm(model = fit)
      # store design matrix
      xlist[[i]] <- anova_tree$X
      # get ANOVA design matrix for testing data and store
      locs <- rpart.predict(as.party(fit), newdata = test, type = "node")
      testdata[[i]] <- cbind(anova_tree$treeMatrix[as.character(locs), ])
      indices[, i] <- bootstrap_sample
    }
  }

  # return matrix of predictions, list of ANOVA matrices for both testing
  # and training data
  return(list(
    "predictions" = predictions,
    "xlist" = xlist,
    "testdata" = testdata,
    "indices" = indices))
}
