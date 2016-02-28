library(rpart)
setwd("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012 Advanced Computational Methods 2/datasets/Spam")


data <- read.csv("spambase.data", header = TRUE, sep = ",") 
data$label <- data$X1
data$X1 <-NULL
xnam <- paste("x", 1:57, sep="")   # rename the variables
colnames(data) <- c(xnam, "y")
fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))  # get the right formula to call the function

# Due to time issues, I will run the algorithm just with 50 observations that will be split in training and test
set.seed(5)                
data<- data[sample(nrow(data), 50), ]
train <- data[1:40, ]
test <- data[-(1:40),-58]
y <- data[-(1:40), 58]  # get the right labels

train.pack <- rep(NA, 3)
test.pack <- rep(NA,3)
train.mine <- rep(NA, 3)
test.mine <- rep(NA, 3)

# run the rpart function
a <- rpart(fmla, data = train, method = "class")

# get training fitted values
fitted.values <- a$y -1

# check the training error
train.pack <- rep(sum(train[,58] == fitted.values)/40, 4)

# use the trained model to predict the remaining labels
pred <- predict(a, test)

# get the predicted labels
fitted <- rep(NA, 10)
for( i in 1: 10){
  if(pred[i,1] > pred[i,2]) fitted[i] <- 0
  else fitted[i] <- 1
}

# check the error
test.pack <- rep(sum(fitted == y)/10, 4)


# run a loop to get the error rates wrt depth values of my function: this does not work, so I'll do it manually
#for (depth in 1:3){
 # test.mine[i]<- cTree(fmla, data = train,  depth = depth, minPoints = 3, costFnc = "ME", test = test, type = "predict")$testLabels
  #acc.test[i] <- sum(test.mine[i] == y)/10
  #train.mine[i]<- cTree(fmla, data = train,  depth = depth, minPoints = 3, costFnc = "ME", type = "train")$predictedlabels
  #acc.train[i] <- sum(train.mine[i] == train$y)/40
#}

# Apparently the previous loop does not work, so I'll do it manually

testone <- cTree(fmla, data = train, depth = 1, minPoints = 3, costFnc = "ME", test = test, type = "predict")$testLabels
testtwo <- cTree(fmla, data = train, depth = 2, minPoints = 3, costFnc = "ME", test = test, type = "predict")$testLabels
testthree <- cTree(fmla, data = train, depth = 3, minPoints = 3, costFnc = "ME", test = test, type = "predict")$testLabels
testfour <- cTree(fmla, data = train, depth = 4, minPoints = 3, costFnc = "ME", test = test, type = "predict")$testLabels
accone <- sum(testone == y)/10
acctwo <- sum(testtwo == y)/10
accthree <- sum(testthree == y)/10
accfour <- sum(testfour == y)/10

test.mine <- c(accone, acctwo, accthree, accfour)

# do the same for the train
trainone <- cTree(fmla, data = train, depth = 1, minPoints = 3, costFnc = "ME", type = "train")$predictedlabels
traintwo <- cTree(fmla, data = train, depth = 2, minPoints = 3, costFnc = "ME", type = "train")$predictedlabels
trainthree <- cTree(fmla, data = train, depth = 3, minPoints = 3, costFnc = "ME", type = "train")$predictedlabels
trainfour <- cTree(fmla, data = train, depth = 4, minPoints = 3, costFnc = "ME", type = "train")$predictedlabels
acc.one <- sum(trainone == train$y)/40
acc.two <- sum(traintwo == train$y)/40
acc.three <- sum(trainthree == train$y)/40
acc.four <- sum(trainfour == train$y)/40
 
train.mine <- c(acc.one, acc.two, acc.three, acc.four)



# produce the plot
x <- c(1:4)  # depth values

df <- data.frame(x=rep(x,4), y=c(train.mine, test.mine, train.pack, test.pack), class=c(rep("train.error", 4), rep("test.error", 4), rep("package.train.error", 4), rep("package.test.error", 4)))
library(ggplot2)
ggplot(df, aes(x=x, y=y, color=class)) + geom_line() + xlab("depth") + ylab("accuracy")


