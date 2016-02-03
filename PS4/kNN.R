library(mvtnorm)
library(dplyr)
library(plyr)


#compute the mode of a vector x: it will be needed later
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#function for the KNN
kNN <- function(features, labels, memory, 
                k, p, type) {
  
  # test the inputs
  library(assertthat)
  not_empty(features); not_empty(labels); 
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.count(k);
  is.count(p);
  #assert_that(p %in% c(1, 2, Inf)), uncomment if you want such a restriction on the value of p
  if (type == "predict") {
    assert_that(not_empty(memory) & 
                  ncol(memory) == ncol(features) & 
                  nrow(memory) == length(labels))
  }
  
  # Compute the distance between each point and all others 
  noObs <- nrow(features)
  
  # if we are making predictions on the test set based on the memory, 
  # we compute distances between each test observation and observations
  # in our memory
  if (type == "train") {
    distMatrix <- matrix(NA, noObs, noObs)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = noObs, ncol = ncol(features), 
                              byrow = TRUE)
     
      # computing distances between the probe and exemplars in the
      # training X
      if (p != Inf) {
        distMatrix[obs, ] <- (rowSums((abs(features - 
                                             probeExpanded))^p) )^(1/p)
      } else {
        distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
      }  
    }
  } else if (type == "predict") {
    noMemory <- nrow(memory)
    distMatrix <- matrix(NA, noObs, noMemory)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = noMemory, ncol = ncol(memory), 
                              byrow = TRUE)
      
      # computing distances between the probe and exemplars in the memory
      if (p != Inf) {
        distMatrix[obs, ] <- (rowSums((abs(memory - 
                                             probeExpanded))^p) )^(1/p)
      } else {
        distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
      }  
    }
  }
  
  # Sort the distances in increasing numerical order and pick the first 
  # k elements
  neighbors <- apply(distMatrix, 1, order) 
  
  # Compute the frequency of the assigned class in the k nearest neighbors and return the vector of probabilities
  prob <- rep(NA, nrow =noObs)
  predLabels <- rep(NA, noObs)
  for (obs in 1:noObs) {
    # predicted label
    x <- as.vector(labels[neighbors[1:k, obs]])
    predLabels[obs] <- as.numeric(names(sort(-table(x)))[1]) #computes the mode of the labels of the k-nn
    
    #frequency of the predicted label among the k-NN
    prob[obs] <- max(count(labels[neighbors[1:k, obs]])$freq)/k
    
  }
  
  # return the results
  return(list(predLabels = predLabels, 
              prob = prob
              ))
}




# 2 class 2 dimensional mixture of Gaussians
genGaussMix <- function(noObs = c(100, 100), 
                        noGaussians = 10, 
                        mixtureProb = rep(1/noGaussians, noGaussians), 
                        seed = 2222) {
  
  
  # producing means of our bivariate Gaussians
  meansC1 <- rmvnorm(noGaussians, mean = c(1,0), sigma = diag(2))
  meansC2 <- rmvnorm(noGaussians, mean = c(0,1), sigma = diag(2))
  
  # for each observation we first randomly select one Gaussian and then 
  # generate a point according to the parameters of that Gaussian
  whichGaussianC1 <- sample(nrow(meansC1), noObs[1], 
                            mixtureProb, replace = TRUE)
  whichGaussianC2 <- sample(nrow(meansC2), noObs[2], 
                            mixtureProb, replace = TRUE)
  
  # now drawing samples from selected bivariate Gaussians
  drawsC1 <- whichGaussianC1 %>% 
    sapply(function(x) rmvnorm(1, mean = meansC1[x,], 
                               sigma = diag(2)/5)) %>% t()
  drawsC2 <- whichGaussianC2 %>% 
    sapply(function(x) rmvnorm(1, mean = meansC2[x,], 
                               sigma = diag(2)/5)) %>% t()
  
  # combining and labeling
  dataset <- data.frame(rbind(drawsC1, drawsC2), 
                        label = c(rep("C1", noObs[1]), rep("C2", noObs[2])), 
                        y = c(rep(0, noObs[1]), rep(1, noObs[2])),
                        stringsAsFactors = FALSE)
  return(dataset)
}


dataset <- genGaussMix()

a <- kNN(dataset[,1:2], dataset[,4], k = 7, p= 2, type = "train")
a$predLabels
a$prob

table(a$predLabels, dataset$y)
