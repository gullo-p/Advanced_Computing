#get the data
MNIST_train <- read.csv("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012_Advanced_Computational_Methods/datasets/MNIST/MNIST_training.csv", header = FALSE, sep =",")

MNIST_test <- read.csv("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012_Advanced_Computational_Methods/datasets/MNIST/MNIST_test.csv", header = FALSE, sep =",")

#needed packages
if (!require("kknn")) install.packages("kknn")
library(kknn)
?kknn

#construct the input for knn function

train <- MNIST_train[, 2:257] 
test <- MNIST_test[,1:256]
cl <- MNIST_train[,1]
predictedClasses <- kknn(MNIST_train[,1] ~., MNIST_train[, 2:256] , MNIST_test[ ,1:256], k = 15, distance = 2, kernel = "rectangular")
head(predictedClasses$prob)


### OPTIMIZATION OF k and p: 
## Idea: first fix p = 2 and look for the best k maximizing the accuracy resulting.
## After having chosen the best k we can optimize for p using the same technique.

k <- c(3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 ); 
p <- 2



errorTest <- rep(NA, length(k))

#do the first iteration separately to store the distMatrix, neighbors matrix and update the control value
a <- kNN( X = MNIST_train[,2:257], 
          y = MNIST_train[,2:257],
          memory = MNIST_test[,1:256], 
          k = 3, p = p, 
          type = "predict")
predictedClasses <- a$predLabels

#store the needed distances and neighbors

control <- a$control
distMatrix <- a$distMatrix
neighbors <- a$neighbors

errorTrain[1] <- mean(predictedLabels!=datasetTest[,4])


for (iter in 2:length(k)) {
  # get the test error
  predictedClasses <- kNN( X = MNIST_train[,2:257], 
                           y = MNIST_train[,2:257],
                           memory = MNIST_test[,1:256], 
                           k = k[iter], p = p, 
                           type = "predict")$predictedClasses
  errorTest[iter] <- mean(predictedClasses!=datasetTest[,4])
}

optimalk <- which.min(errorTest) #gives the index of the minimum error, so our optimal k

#NOW optimize on p
k <- optimalk
p <- c(1:100, Inf)

for (iter in 1:length(p)) {
  # get the test error
  predictedClasses <- kNN( X = MNIST_train[,2:257], 
                           y = MNIST_train[,2:257],
                           memory = MNIST_test[,1:256], 
                           k = k, p = p[iter], 
                           type = "predict")$predictedClasses
  errorTest[iter] <- mean(predictedClasses!=datasetTest[,4])
}

optimalp <- which.min(errorTest)

predictedClasses <- kNN(X = MNIST_train[,2:257], 
                        y = MNIST_train[,2:257],
                        memory = MNIST_test[,1:256], 
                        k = optimalk, p = optimalp, 
                        type = "predict")$predictedClasses

setwd("~/Desktop/BGSE/winter_term/Adv_Computing/Advanced_Computing/PS4")
write.csv(predictedClasses, "MNIST_predictions.csv", row.names = FALSE)
