library(rpart)
setwd("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/15D012 Advanced Computational Methods 2/datasets/Spam")


data <- read.csv("spambase.data", header = TRUE, sep = ",") 
data$label <- data$X1
data$X1 <-NULL
xnam <- paste("x", 1:57, sep="")   # rename the variables
colnames(data) <- c(xnam, "y")
fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))  # get the right formula to call the function

# Due to time issues, I will run the algorithm just with 200 observations that will be split in training and test
set.seed(5)                
data<- data[sample(nrow(data), 200), ]
train <- data[1:150, ]
test <- data[-(1:150),-58]
y <- data[-(1:150), 58]  # get the right labels

train.pack <- rep(NA, 6)
test.pack <- rep(NA,6)
train.mine <- rep(NA, 6)
test.mine <- rep(NA, 6)

# run the rpart function
a <- rpart(fmla, data = train, method = "class")

# get training fitted values
fitted.values <- a$y -1

# check the training error
train.pack <- rep(sum(train[,58] == fitted.values)/150, 6)

# use the trained model to predict the remaining labels
pred <- predict(a, test)

# get the predicted labels
fitted <- rep(NA, 50)
for( i in 1: 50){
  if(pred[i,1] > pred[i,2]) fitted[i] <- 0
  else fitted[i] <- 1
}

# check the error
test.pack <- rep(sum(fitted == y)/50, 6)

# run a loop to get the error rates wrt depth values of my function
for (depth in 1:6){
  test.mine[i]<- sum(cTree(fmla, data = train,  depth = depth, minPoints = 3, costFnc = "ME", test = test, type = "predict")$testLabels ==y)/50
  train.mine[i]<- sum(cTree(fmla, data = train,  depth = depth, minPoints = 3, costFnc = "ME", type = "train")$testLabels == train$y)/150
}


 
 
 
# produce the plot
x <- c(1:6)
plot(x, test.mine, type ="l", col ="red")
lines(x, train.mine, type ="l", col="blue")
lines(x, test.pack, type = "l", col ="green")
lines(x, train.pack, type = "l", col ="yellow")



y1 <- c(100, 200, 300, 400, 500)
y2 <- c(1, 2, 3, 4, 5)

df <- data.frame(x=rep(x,4), y=c(train.mine, test.mine, train.pack, test.pack), class=c(rep("train.error", 6), rep("test.error", 6), rep("package.train.error", 6), rep("package.test.error", 6)))
