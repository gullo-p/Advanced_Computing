if (!require("rpart")) install.packages("rpart"); library(rpart)
if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
if(!require("gbm")) install.packages("gbm"); library(gbm)

setwd("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012 Advanced Computational Methods/datasets/Spam")

# load data
data <- read.csv("spambase.data", header = TRUE, sep = ",") 
data$label <- data$X1
data$X1 <-NULL

# put the labels in the correct format: either -1 or +1
data$label[which(data$label == 0)] <- -1

# rename the features
xnam <- paste("x", 1:57, sep="")   
colnames(data) <- c(xnam, "y")

# define the formula we will use to call the adaBoost function
fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))  

# get the labels
Y.var <- get.vars(lhs(fmla))      
y <- data[,Y.var]

# create train and test data subsampling from the dataset
bound <- floor((nrow(data)/4)*3)         #define % of training and test set

df <- data[sample(nrow(data)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]

# adaboost from gbm package wants the labels in {0,1}. Thus I will transform them again
df2 <- df
df.train2 <- df.train
df.test2 <- df.test
df.train2$y[which(df.train2$y == -1)] <- 0
df.test2$y[which(df.test2$y == -1)] <- 0

# initialize two error vectors for training and test error for each of the two functions - mine or R package one
iter <- 50     # number of iterations
err_train <- rep(NA, iter)
err_test <- rep(NA, iter)
err_trainR <- rep(NA, iter)
err_testR <- rep(NA, iter)

# run the loop for computing the errors
for(i in 1:iter){
  
  # fit the models
  a <- adaBoost(fmla, df.train, 2, i, type= "train")
  b <- adaBoost(fmla, df.train, 2, i, type = "predict", df.test)
  
  # compute the errors
  err_train[i] <- sum(a$predLabels != df.train[,58])/nrow(df.train)
  err_test[i] <- sum(b$predLabels != df.test[,58])/nrow(df.test)
  
  # use the adaboost function from package
  adaboost <- gbm(formula = fmla, distribution = "adaboost",
                              data = df.train2,
                              n.trees = i,
                              interaction.depth = 2)
  
  err_trainR[i] <- mean((predict(adaboost, df.train2, n.trees = i) > 0) != df.train2$y)
  err_testR[i] <- mean((predict(adaboost, df.test2, n.trees = i) > 0) != df.test2$y)
  
  
}



# set up the plot
x <- c(1:50)  # iterations values

df <- data.frame(x=rep(x,4), y=c(err_train, err_test, err_trainR, err_testR), class=c(rep("train.error", 50), rep("test.error",50), rep("package.train.error", 50), rep("package.test.error", 50)))

library(ggplot2)
ggplot(df, aes(x=x, y=y, color=class)) + geom_line() + xlab("noTrees") + ylab("Misclassification rate")


