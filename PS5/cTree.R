if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
if (!require("compiler")) install.packages("compiler");library(compiler)

library(plyr)


# define the three possible loss functions
ME <- function(prob) { 
  MissError <- 1 - apply(prob,2,max) 
  return(MissError)
}

Gini <- function(prob) {
  Gini <- colSums(prob*(1-prob)) 
  return(Gini)
}

Entropy <- function(prob) {
  for(i in 1:nrow(prob)){
    for(j in 1:ncol(prob)){
      if(prob[i,j] ==0) prob[i,j] <- 0.00000001   # if there are 0's replace them with small numbers
    }
  }
  CrossEntropy <-  -colSums(prob*log(prob)) 
  return(CrossEntropy)
}


# lets first develop a function that will partition the input space, 
# cut the input space exhaustively, count errors for each cut and
# choose the best cut

findThreshold <- function(X, y, method) { 
  
  noObs <- nrow(X)
  numfeatures <- ncol(X)
  misError <- rep(NA, 2)     
  splitLabels <- rep(NA,2)
  predictedClasses <- matrix(NA, ncol = numfeatures, nrow = noObs) # for every feature record the predicted classes of the observations
  freqClasses <- matrix(NA, ncol = 2, nrow = length(unique(y)))  # this matrix stores for each class the frequency of that class in the left and the right split
  splitprob <- rep(NA,2)
  # initialize the first value for besterr with something very big (so that we can certainly improve it)
  besterr <- 100000 
  
  i <- 46
  idx <- 600
  # for each feature compute the best split
  for (i in 1:numfeatures){
    print(i)
    # first sort the values for each variable and then define the split
    X_ordered <- X[ order(X[,i]), ]
    
    # we go sequentially over each point and cut between that point and the
    # closest neighbor
    for (idx in 1:(noObs-1)) {
      print(idx)
      # locate a potential threshold for feature i, a split between two points
      potThres <- mean(X_ordered[idx:(idx+1), i])
      
      
      # compute the frequencies of the labels in the split
      if(sum(count(y[X[,i] <= potThres])$freq) == 0){ # if the split is empty dont' do anything and go to the next one
        next} else{
          freqLeft <- count(y[X[,i] <= potThres])$freq/sum(count(y[X[,i] <= potThres])$freq)   # compute the frequency of each label on the left split    
          if(nrow(count(y[X[,i] <= potThres])) < length(unique(y))) {  # if there are some labels with frequency 0, add them to the frequency table
            true <- sort(unique(y))
            table <- count(y[X[,i] <= potThres])
            lab <- table$x
            missinglab <- setdiff(true, lab)
            missing <- cbind(missinglab, 0)   # adds frequency 0 for each of the missing label
            colnames(missing) <- c("x", "freq")
            table <- rbind(table,missing)
            freqLeft <- table$freq/sum(count(y[X[,i] <= potThres])$freq)
          } 
          
        }
      
      
      # same for the right split
      if(sum(count(y[X[,i] > potThres])$freq) == 0){ # if the split is empty don't do anything and go to the next one
        next} else{
          freqRight <- count(y[X[,i] > potThres])$freq/sum(count(y[X[,i] > potThres])$freq)   
          if(nrow(count(y[X[,i] > potThres])) < length(unique(y))) {  # if there are some labels with frequency 0, add them to the frequency table
            true <- sort(unique(y))
            table <- count(y[X[,i] > potThres])
            lab <- table$x
            missinglab <- setdiff(true, lab)
            missing <- cbind(missinglab, 0)   # adds frequency 0 for each of the missing label
            colnames(missing) <- c("x", "freq")
            table <- rbind(table,missing)
            freqRight <- table$freq/sum(count(y[X[,i] > potThres])$freq) 
          } 
          
        }
      
      
      freqClasses <- cbind(freqLeft, freqRight)
      
      # compute the predicted classes in the left and right split
      lableft <- y[X[,i] <= potThres]   
      labright <- y[X[,i] > potThres]
      modeleft <- as.numeric(names(sort(-table(lableft)))[1]) # compute the mode label in left and right split
      moderight <- as.numeric(names(sort(-table(labright)))[1])
      
      predictedClasses[X[,i] <= potThres, i] <- modeleft   # store the predicted classes for this split for feature i
      predictedClasses[X[,i] > potThres, i] <- moderight
      
      # save the probabilities for the predicted labels
      probleft <- max(freqLeft) 
      probright <- max(freqRight)
      
      # error of this split: this call gives the error on the two subsets defined by the current split
      if(method == "Entropy"){
        misError <- Entropy(freqClasses)
      } else if(method == "Gini"){
        misError <- Gini(freqClasses)
      } else if(method == "ME"){
        misError <- ME(freqClasses)
      }
      
      # total loss
      err <- sum(misError)
      
    
      # computing the accuracy, thresholds and labels of 
      # the splitted interval and updating them if we find a better split.
      if(err < besterr){
        besterr <- err
        bestThreshold <- potThres
        splitLabels <- c(predictedClasses[X[,i] <= bestThreshold, i],
                                predictedClasses[X[,i] > bestThreshold, i])
        splitprob <- c(probleft, probright)
        bestfeature <- i
      }
      
    }
    
  }  
  
  # what are the final labels of the best split?
  labels <- splitLabels
  prob <- splitprob

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
  if (nrow(X) < minPoints) return(list(predictedlabels = labels, 
                                       misError = err,
                                       fittedprob = prob)) 
   
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




findThreshold <- cmpfun(findThreshold)
