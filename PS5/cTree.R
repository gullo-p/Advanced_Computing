if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
library(plyr)

# define the three possible loss functions
ME <- function(prob) { 
  MissError <- 1 - apply(prob,1,max) 
  return(MissError)
}


Gini <- function(prob) {
  Gini <- rowSums(prob*(1-prob)) 
  return(Gini)
}

Entropy <- function(prob) {
  CrossEntropy <- - rowSums(prob*log(prob)) 
  return(CrossEntropy)
}



y <- c(1,2,3,3,4,1)
z <- c(2,2,2,3,4,1)
x <- count(y)$freq/6
x
s <- count(z)$freq/6
cbind(x,s)

# lets first develop a function that will partition the input space, 
# cut the input space exhaustively, count errors for each cut and
# choose the best cut

findThreshold <- function(X, y) { 
  
  noObs <- nrow(X)
  errors <- matrix(NA, nrow = noObs-1, ncol = numfeatures) # a matrix that will store errors (either Gini, entropy or ME) for each split and for each feature
  thresholds <- rep(NA, noObs-1)
  splitLabels <- matrix(NA, ncol=2, nrow=noObs-1)
  numfeatures <- ncol(X)
  predictedClasses <- matrix(NA, ncol = numfeatures, nrow = noObs) # for every feature record the predicted classes of the observations
  freqClasses <- matrix(NA, ncol = 2, nrow = length(unique(y)))  # this matrix stores for each class the frequency of that class in the left and the right split
  
  # for each feature compute the best split
  for (i in 1:numfeatures){
    
    # we go sequentially over each point and cut between that point and the
    # closest neighbor
    for (idx in 1:(noObs-1)) {
    
      # locate a potential threshold for feature i, a split between two points
      potThres <- mean(X[idx:(idx+1), i])
    
      # compute the frequencies of the labels in the split
      freqLeft <- count(y[X[,i] < potThres]$freq)/idx   # compute the frequency of each label on the left split
      freqRight <- count(y[X[,i] > potThres]$freq)/(noObs - idx)  # same for right split
      freqClasses <- cbind(freqLeft, freqRight)
      
      # compute the predicted classes in the left and right split
      lableft <- as.vector(y[X[,i] < potThres])
      labright <- as.vector(y[X[,i] > potThres])
      modeleft <- as.numeric(names(sort(-table(lableft)))[1]) # compute the mode label in left and right split
      moderight <- as.numeric(names(sort(-table(labright)))[1])
      
      predictedClasses[X[,i] < potThres, i] <- modeLeft   # store the predicted classes for this split for feature i
      predictedClasses[X[,i] > potThres, i] <- modeRight
    
        
      
      # error of this split
      misError <- method()
    
      # recording the accuracy, thresholds and labels of 
      # the splitted interval
      errors[idx, i] <- misError
      thresholds[idx] <- potThres
      splitLabels[idx,] <- c(predictedClasses[x < potThres][1],
                             predictedClasses[x > potThres][1])
    }
  }  
  # print(cbind(errors, thresholds, splitLabels))
  
  # next we find the minimum and the best threshold
  minError <- min(errors)
  bestThreshold <- thresholds[which(errors==minError)]
  # if more than 1 threshold has the same accuracy we choose one randomly
  bestThreshold <- sample(bestThreshold, 1)
  
  # what are the final labels of the best split?
  labels <- splitLabels[which(thresholds==bestThreshold),]
  # print(cbind(minError, bestThreshold, labels))
  
  return(list(thres = bestThreshold, 
              err = minError, 
              labels = labels))
}


# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
# based on a global error

CTree <- function(X, y,  data, depth, minPoints, costFnc) { # poi modifica X e y in formula
  
  # setting up the initial boundaries - whole interval, with each
  # iteration of k this will expand
  boundaries <- c(0, 1)
  
  # iterating for K times, i.e. we iteratively split input space in an 
  # empirically greedy way, keeping only the best split and adding it 
  # to the boundaries, we stop after doing this K times
  for (k in 1:K) {
    
    # first we subset our input space according to the boundaries 
    # found so far
    intervals <- cut(X, boundaries, include.lowest = TRUE)
    noIntervals <- length(levels(intervals))
    
    # then we go over each subset and see what is the best splitting
    # point locally in this subset, using the findThreshold function
    thresholds <- rep(NA, noIntervals)
    errors <- rep(NA, noIntervals)
    splitLabels <- matrix(NA, ncol=2, nrow=noIntervals)
    for (iter in 1:noIntervals) {
      x <- X[intervals==levels(intervals)[iter]]
      y <- Y[intervals==levels(intervals)[iter]]
      # we skip if there is a single element in the interval
      # nothing to split there
      if (length(y)>1) {
        # find the local splitting point
        results <- findThreshold(x, y)
        thresholds[iter] <- results$thres
        splitLabels[iter,] <- results$labels
        
        # add the potential threshold to our list of boundaries
        boundariesHH <- c(boundaries, abs(results$thres))
        boundariesHH <- sort(boundariesHH)
        
        # add the signs of the new threshold which indicates what 
        # is the label of the of newly splitted interval
        if (k==1) {
          signsHH <- results$labels
        } else {
          signsHH <- append(signs, results$labels[1], 
                            after=which(boundariesHH==results$thres)-2)
          signsHH[which(boundariesHH==results$thres)] <- 
            results$labels[2]
        }
        
        # now we compute predictions with new boundaries based on the 
        # potential split
        predictedClasses <- cut(X, boundariesHH)
        levels(predictedClasses) <- signsHH 
        
        # we compute a global, overall error rate for this local
        # modification, we do not use the local error to evaluate 
        # the splitting point
        errors[iter] <- mean(predictedClasses != Y)
      }
    }
    
    # find the best threshold in this iteration, greedy strategy
    minError <- min(errors, na.rm=TRUE)
    bestThreshold <- thresholds[which(errors==minError)]
    bestThreshold <- sample(bestThreshold, 1)
    labels <- splitLabels[which(thresholds==bestThreshold),]
    
    # add the new threshold to our list of boundaries
    boundaries <- c(boundaries, abs(bestThreshold))
    boundaries <- sort(boundaries)
    
    # add the signs of the new threshold which indicates what is the label of the newly splitted interval
    if (k==1) {
      signs <- labels
    } else {
      signs <- append(signs, labels[1], 
                      after=which(boundaries==bestThreshold)-2)
      signs[which(boundaries==bestThreshold)] <- labels[2]
    }
  }
  
  # get the final predicted classes
  predictedClasses <- cut(X, boundaries)
  levels(predictedClasses) <- signs 
  
  # now we evaluate the final accuracy, after K iterations
  misError <- mean(predictedClasses != Y)
  
  
  return(list(predictedClasses = predictedClasses, 
              misError = misError,
              boundaries = boundaries,
              signs = signs))
}
