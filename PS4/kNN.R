library(mvtnorm)
library(dplyr)
library(plyr)
if(!require("flexclust")) install.packages("flexclust"); library(flexclust)


#Function for the KNN. 
#Remark: control is a dummy, which is set to TRUE if distMatrix has already been created,
#so that in future calls of the function (important when optimizing over k and p) it won't be calculated again.
kNN <- function(features, labels, memory, 
                k, p, type, control) {
  
  # test the inputs
  library(assertthat)
  not_empty(features); not_empty(labels); 
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.count(k);
  assert_that(p %in% c(1: 100, Inf))
  assert_that(k %% 2 != 0)   #check that k is odd - just not to have any tie in the decisions
  if (type == "predict") {
    assert_that(not_empty(memory) & 
                  ncol(memory) == ncol(features))
    assert_that(k <= nrow(features)) #the number of nearest neighbors must not exceed the total number of observations
  }
  
  # Compute the distance between each point and all others using dist function if type = train,
  # otherwise using dist2, which is the row-wise distance between two matrices.
  noObs <- nrow(features)
  noMemory <- nrow(memory)
  
  if(control == FALSE){
    
    
    if (type == "train") {
      distMatrix <- matrix(NA, noObs, noObs)
      
      if (p != Inf) {
        distMatrix <- as.matrix(dist(features, method ="minkowski", diag = TRUE, upper = TRUE, p = p))
      } else {
        distMatrix <- as.matrix(dist(features, method ="maximum", diag = TRUE, upper = TRUE))
      }  
    } else if (type == "predict") {
       
       distMatrix <- matrix(NA, noObs, noMemory)
      
      
       if (p != Inf) {
         distMatrix <- as.matrix(dist2(memory, features, method = "minkowski", p=p))
       } else {
         distMatrix <- as.matrix(dist2(memory, features, method = "maximum"))
       }  
    }
    
  }    
  # Sort the distances in increasing numerical order and pick the first 
  # k elements
  neighbors <- apply(distMatrix, 1, order)

  #update the value of control so that in future calls we don't run the first part of the function
  control <- TRUE
  
  if(type == "train"){
    # Compute both the predicted label and the frequency of the assigned class in the k nearest 
    #neighbors and return the two resulting vectors 
    prob <- rep(NA, noObs)
    predLabels <- rep(NA, noObs)
    for (obs in 1:noObs) {
      # predicted label
      x <- as.vector(labels[neighbors[1:k, obs]])
      predLabels[obs] <- as.numeric(names(sort(-table(x)))[1]) #computes the mode of the labels of the k-nn
    
      #frequency of the predicted label among the k-NN
      prob[obs] <- max(count(labels[neighbors[1:k, obs]])$freq)/k
    
    }
  } else if(type == "predict"){
    prob <- rep(NA, noMemory)
    predLabels <- rep(NA, noMemory)
    for(obs in 1:noMemory){
      # predicted label
      x <- as.vector(labels[neighbors[1:k, obs]])
      predLabels[obs] <- as.numeric(names(sort(-table(x)))[1]) #computes the mode of the labels of the k-nn
      
      #frequency of the predicted label among the k-NN
      prob[obs] <- max(count(labels[neighbors[1:k, obs]])$freq)/k
      
    }
  }
  
  # return the results 
  return(list(predLabels = predLabels, 
              prob = prob,
              distMatrix = distMatrix,
              neighbors = neighbors,
              control = control
  ))
}


