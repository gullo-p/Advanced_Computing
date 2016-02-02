setwd("/Users/guglielmo/Desktop/BGSE/winter_term/Adv_Computing/Advanced_Computing/PS4") #change the path to the directory PS4 of the github repo

source("kNN.R")


# ----
# Denitsa's spirals 
# ----

genSpirals <- function(N = 200,
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


#create the data to test
data <- genSpirals()

#call the kNN function and get predLabels and prob
a <- kNN(dataset[,1:2], dataset[,4], data[,1:2], k = 5, p = 2, type = "predict")
realdata <- cbind(data, a$predLabels, a$prob)

#save in the right format in a csv file
colnames(realdata) <- c("X1", "X2", "Y", "predLabels","prob")
write.csv(realdata, "predictions.csv", row.names = FALSE)

#save the pdf file
cairo_pdf("dataPlot.pdf")
print(
  ggplot(data = realdata, 
         aes(x = x1, y = x2, colour=y)) + 
    scale_colour_continuous(guide = FALSE) +
    geom_point() +
    ggtitle("Spirals") +
    xlab("x1") +
    ylab("x2") +
    stat_contour(mapping = predLabels, data = realdata)+
    theme_bw()
)
dev.off()

