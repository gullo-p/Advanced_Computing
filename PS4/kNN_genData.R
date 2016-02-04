
#set working directory that contains "kNN.R" file
setwd("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/Advanced_Computing/PS4") #change the path to the directory PS4 of the github repo

source("kNN.R")


# ----
# Denitsa's spirals 
# ----

genSpirals <- function(N = 500,
                       degrees = 570,
                       location = 90,
                       blend = 0.2,
                       saveData = FALSE, 
                       savePlot = FALSE) {
  
  # Generate two-spiral data 
  # idea of the problematic dataset: http://www.benmargolis.com/compsci/ai/two_spirals_problem.htm
  # N - number of observations
  # degrees - length of the spiral 
  # location - how far away from the origin
  # blend<-blending together
  
  #necessary packages
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  if (!require("akima")) install.packages("akima"); library(akima)
  
  # define some variables
  degrees2rad <- (2*pi)/360 #convert degrees to radiant
  location <- location*degrees2rad #how far away from 00 the spiral starts
  
  N1 <- floor(N/2)
  N2 <- N-N1
  
  #spiral 1 
  #we indicate it by 0 in V3
  n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d1 <- t(rbind(-1*n*cos(n)+runif(N1)*blend, sin(n)*n+runif(N1)*blend, rep(0,N1)))
  
  #the second spiral we indicate by 1 in V3
  n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d2 <-t(rbind(n*cos(n)+runif(N1)*blend, -1*sin(n)*n+runif(N1)*blend, rep(1,N1))) 
  
  #combine the data 
  data <- data.frame(rbind(d1, d2))
  names(data) <- c("x1", "x2", "y")
  
  return(data)
}


#create a dataset for training using runif (will perform bad of course)
#x1 <- as.data.frame(runif(100000, min=-10, max = 10))
#x2 <- as.data.frame(runif(100000, min=-12, max = 12))
#dataset <- cbind(x1, x2, c(rep(0,50000), rep(1,50000)))

#create the data 
data <- genSpirals()

#split randomly into training and test data
bound <- floor((nrow(data)/4)*3)         #define % of training and test set

df <- data[sample(nrow(data)), ]           #sample rows 
df.train <- df[1:bound, 1:2]                       #get training set
df.test <- df[(bound+1):nrow(df), 1:2]
df.label <- df[1:bound, 3]
df.labtest <- df[(bound+1):nrow(df), 3]


#call the kNN function and get predLabels and prob: we use k = 35 to get non-trivial results
a <- kNN(features = df.train, labels = df.label, memory = df.test, k = 35, p = 2, type = "predict", control = FALSE)
realdata <- cbind(df.test, df.labtest, a$predLabels, a$prob)


#save in the right format in a csv file
colnames(realdata) <- c("X1", "X2", "Y", "predLabels","prob")
write.csv(realdata, "predictions.csv", row.names = FALSE)
table(a$predLabels, df.labtest)


#produce the grid to get the stat_contour
grid <- interp(as.data.frame(realdata)$X1, as.data.frame(realdata)$X2, 
               as.data.frame(realdata)$predLabels)
grid2 <- expand.grid(x=grid$x, y=grid$y)
grid2$z <- as.vector(grid$z)
grid2$z[grid2$z > 0.5] <- 1


#save the pdf file
plot.pdf <- function(realdata){
  pdf("plot.pdf", width=4, height=4.5)
  a <- ggplot(data = realdata, 
              aes(x = X1, y = X2, colour=Y, z = Y)) + 
    geom_point() +
    ggtitle("Spirals") +
    xlab("x1") +
    ylab("x2") +
    theme_bw()+
    stat_contour(data=na.omit(grid2), binwidth=1, 
                 colour="red", aes(x=x, y=y, z=z))
  print(a)
  dev.off()
}
plot.pdf(realdata)
