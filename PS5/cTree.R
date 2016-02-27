if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
library(plyr)


# define the three possible loss functions
ME <- function(prob) { 
  MissError <- 1 - apply(prob,2,max) 
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


# lets first develop a function that will partition the input space, 
# cut the input space exhaustively, count errors for each cut and
# choose the best cut

findThreshold <- function(X, y) { 
  
  noObs <- nrow(X)
  numfeatures <- ncol(X)
  miserror <- rep(NA, 2)     
  #errors <- matrix(NA, nrow = noObs-1, ncol = numfeatures) # a matrix that will store errors (either Gini, entropy or ME) for each split and for each feature
  #thresholds <- rep(NA, nrow = noObs-1, ncol = numfeatures)
  splitLabels <- rep(NA,2)
  predictedClasses <- matrix(NA, ncol = numfeatures, nrow = noObs) # for every feature record the predicted classes of the observations
  freqClasses <- matrix(NA, ncol = 2, nrow = length(unique(y)))  # this matrix stores for each class the frequency of that class in the left and the right split
  besterr <- 10000000  # initialize the first value for besterr with something very big (so that we can certainly improve it)
  
  
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
    
      # save the probabilities for the predicted labels
      probleft <- length(y[which(y[] == modeleft)])/length(y)
      probright <- length(y[which(y[] == moderight)])/length(y)
      
      # error of this split: this call gives the error on the two subsets defined by the current split
      misError <- method(freqClasses)
      
      # total loss
      if(method == Entropy | method == Gini){
        err <- (idx/noObs)*misError[1] + ((noObs - idx)/noObs)*misError[2]
      } else if(method == ME){
        err <- misError[1] + misError[2]
      }
    
      # computing the accuracy, thresholds and labels of 
      # the splitted interval and updating them if we find a better split.
      if(err < besterr){
        besterr <- err
        bestThreshold <- potThres
        splitLabels <- c(predictedClasses[X[,i] < bestThreshold, i],
                                predictedClasses[X[,i] > bestThreshold, i])
        splitprob <- c(probleft, probright)
        bestfeature <- i
      }
      #errors[idx, i] <- err
      #thresholds[idx, i] <- potThres
      #splitLabels[idx,i] <- c(predictedClasses[X[,i] < potThres, i][1],
       #                       predictedClasses[X[,i] > potThres, i][1])
    }
    
  }  
  # print(cbind(errors, thresholds, splitLabels))
  
  # next we find the minimum and the best threshold
  #minError <- rep(NA, numfeatures)
  #minError <- apply(errors, 2, min)  # stores the minimum error done in each feature
  #bestThreshold <- thresholds[which(errors==minError)]
  
  # if more than 1 threshold has the same accuracy we choose one randomly
  #bestThreshold <- sample(bestThreshold, 1)
  
  # what are the final labels of the best split?
  labels <- splitLabels
  prob <- splitprob
  # print(cbind(minError, bestThreshold, labels))
  
  return(list(thres = bestThreshold, 
              err = besterr, 
              labels = labels,
              feat = bestfeature,
              prob = splitprob))
}


# next we develop the main function that will use findThreshold function
# calling it recursively on the splitted regions

CTree <- function(formula, data, depth, minPoints, costFnc) { 
  
  X <- get.vars(rhs(formula))   # get the features from the formula 
  y <- get.vars(lhs(formula))   # get the labels
  
  if (depth == 0) return()
  if (nrow(X) < minPoints) return() 
   
  a <- findThreshold(X,y)
  feat <- a$feat                # get the feature that produces the best split
  thres <- a$thres              # get the (horizontal) threshold that produces the best split
  prob <- a$prob                # get the fitted probabilites
  labels <- a$labels            # get the labels
  err <- a$err                  # get the misclassification error  

  
  X1 <- X[which(X[,feat] < thres), ]    # define the two subsets for calling recursively the function
  X2 <- X[which(X[,feat] > thres), ]
  y1 <- y[which(X[,feat] < thres)]      # save their labels
  y2 <- y[which(X[,feat] > thres)]
  
  data1 <- data.frame(cbind(X1,y1))     # define the new data frames
  data2 <- data.frame(cbind(X2,y2))
  
  # update the depth
  depth <- depth - 1                
  
  
  # call the function recursively on the two subsets just created
  tree1 <- CTree(y1 ~ X1, data1, depth, minPoints, costFnc)
  tree2 <- CTree(y2 ~ X2, data2, depth, minPoints, costFnc)
  
  
  return(list(predictedlabels = labels, 
              misError = err,
              fittedprob = prob))
}
