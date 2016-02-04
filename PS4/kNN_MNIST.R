#get the data from box folder
MNIST_train <- read.csv("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012_Advanced_Computational_Methods/datasets/MNIST/MNIST_training.csv", header = FALSE, sep =",")

MNIST_test <- read.csv("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012_Advanced_Computational_Methods/datasets/MNIST/MNIST_test.csv", header = FALSE, sep =",")


#select the right working directory which has kNN.R function

setwd("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/Advanced_Computing/PS4") #change the path to the directory PS4 of the github repo

#source the function needed
source("kNN.R")
library(class)

### OPTIMIZATION OF k and p: 
## Idea: split the training data in 75% training set and 25% test set and perform 
#cross-validation, thus identifying the best k (fixing for e.g. p <- 2).
## After having chosen the best k we can optimize for p using the same technique.

k <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151); 
p <- 2


#initialize a matrix that computes the error of the test for each value of k and each sample
errorTest <- matrix(NA, nrow = length(k), ncol = 5)

#start the experiment
for (i in 1:5) {    #repeat the experiment for 5 different split of the data
  bound <- floor((nrow(MNIST_train)/4)*3)         #define % of training and test set
  
  df <- MNIST_train[sample(nrow(MNIST_train)), ]           #sample rows 
  df.train <- df[1:bound, 2:257]                       #get training set
  df.test <- df[(bound+1):nrow(df), 2:257]
  df.label <- df[1:bound, 1]
  df.labtest <- df[(bound+1):nrow(df), 1]
  
  for(iter in 1:length(k)){
    
    a <- as.vector(knn( train = df.train, 
                        cl = df.label,
                        test = df.test, 
                        k = k[iter]))
    
    
    errorTest[iter, i] <- mean(a!=df.labtest)  
  }
  
}

#now, let's store in a vector of length 5 the optimal k resulting from the previous function
optimalk <- rep(NA, 5)

for(i in 1:5){
  optimalk[i] <- which.min(errorTest[ ,i]) #gives the index of the minimum error, so our optimal k
}

#THIS FIRST TEST WOULD SUGGEST TO GO WITH k = 1, but it doesn't seem so truthful
#We also try the following rule of thumb taking k = sqrt(n), n being the number of observations.
k <- floor(sqrt(6000))

b <- knn(train = df.train, cl = df.label, test = df.test, k = k)
errorTest2 <- mean(b!=df.labtest)
#which gives a worse result than k = 1.

#Now, to optimize also with respect to p we run three different experiments with k = 13 for p=1,2 and Inf, 
#using the kNN function previously created.
p <- 1
a <- rep(NA, 1500)
error2 <- rep(NA, 5)

bound <- floor((nrow(MNIST_train)/4)*3)         #define % of training and test set

df <- MNIST_train[sample(nrow(MNIST_train)), ]           #sample rows 
df.train <- df[1:bound, 2:257]                       #get training set
df.test <- df[(bound+1):nrow(df), 2:257]
df.label <- df[1:bound, 1]
df.labtest <- df[(bound+1):nrow(df), 1]

###first case: p =1 and k = 13
a <- kNN(features = df.train, 
         labels = df.label,
         memory = df.test, 
         k = 11, p = 1, type = "predict", control = FALSE)
error2 <-  mean(a$predLabels!=df.labtest)

##record distMatrix and neighbors matrix so that the future calls of kNN will be extremely fast!
distMatrix <- a$distMatrix
neighbors <- a$neighbors

#second case: p = 2
a <- kNN(features = df.train, 
         labels = df.label,
         memory = df.test, 
         k = 3, p = 2, type = "predict", control = TRUE)$predLabels
error3 <-  mean(a!=df.labtest)

#third case: p = Inf
a <- kNN(features = df.train, 
         labels = df.label,
         memory = df.test, 
         k = 3, p = Inf, type = "predict", control = TRUE)$predLabels
error4 <-  mean(a!=df.labtest)

#from the results we get that p = 2 (or p=Inf) is the best choice.



##Final run of the knn algorithm using k = 1 and p= 2 (as by default in class package)
result <- knn(train = MNIST_train[,2:257], test = MNIST_test[,1:256], cl = MNIST_train[,1], k =1)

result <- as.data.frame(result)

#write everything in a csv file
write.csv(result, file = "MNIST_predictions.csv", quote = FALSE, row.names = FALSE)


