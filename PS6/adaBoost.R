if (!require("rpart")) install.packages("rpart"); library(rpart)
if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
if (!require("plyr")) install.packages("plyr"); library(plyr)


# let's first define our general weak learner
weak <- function(formula, data, w, depth) {
  
  environment(formula) <- environment()
  rpart(formula, data, weights = w, control = rpart.control(maxdepth = depth))

}

# now let's create our adaBoost function
adaBoost <- function(formula, data, depth, noTrees, type, test = NULL){
  
  # assert the inputs are in a correct format
  assert_that(not_empty(data))
  assert_that(is.count(depth))
  assert_that(is.count(noTrees))
  is.formula(formula)
  is.string(type); assert_that(type %in% c("train", "predict"));
  
  # save left hand side of the formula (later on we will use the true labels)  
  Y.var <- get.vars(lhs(formula))  

  err <- rep(NA, noTrees)    # initialize the vector of the errors
  w <- rep(1/nrow(data), nrow(data))   # initialize weights and alpha
  alpha <- rep(NA, noTrees)
  pred_in <- matrix(NA, nrow = nrow(data), ncol = noTrees)  # initialize the matrix for the in-sample predictions
  
  if(type == "predict"){
    pred_out <- matrix(NA, nrow = nrow(test), ncol = noTrees) # if predict, also initialize the matrix for the predictions out of sample
  }
  
  for(i in 1:noTrees){
    
    # train the weak learner
    tree <- weak(formula, data, w, depth)
    
    # get predictions for either training or test data
      
    # get the predicted labels and the error
    pred_in[,i] <- predict(tree, data)
    pred_in[which(pred_in[,i] < 0), i] <- -1
    pred_in[which(pred_in[,i] >= 0),i] <- 1
    err[i] <-  sum(w*(as.numeric(data[,Y.var]!=pred_in[,i])))/sum(w)
      
    # update the weights
    alpha[i] <- log((1-err[i])/err[i])
    w <- w* exp(alpha[i] * as.numeric(data[,Y.var]!=pred_in[,i]))
    
    # if type is predict also compute the out of sample predictions
    if(type == "predict"){
      
      pred_out[,i] <- predict(tree, test)
      pred_out[which(pred_out[,i] < 0), i] <- -1
      pred_out[which(pred_out[,i] >= 0),i] <- 1
    
    }
    
    
  }
  
  if(type == "train"){
    
    weighted <- rep(NA, nrow(data))  # initialize a vector for the final predicted labels
    for(i in 1:nrow(data)){
      weighted[i] <- sum(alpha*pred_in[i,])   # compute the weighted predictions for each observation
    }
    
  } else if(type == "predict"){
    
    weighted <- rep(NA, nrow(test))  # initialize a vector for the final predicted labels
    for(i in 1:nrow(test)){
      weighted[i] <- sum(alpha*pred_out[i,])   # compute the weighted predictions for each observation
    }
    
  }
  
  predLabels <- sign(weighted)           # final prediction is given by the sign of the weighted predictions
  predLabels[which(predLabels == 0)] <- 1  # break the ties
  
  return(list(predLabels = predLabels))
}


