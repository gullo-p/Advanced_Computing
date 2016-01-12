if (!require("extrafont")) install.packages("extrafont"); library(extrafont) 
library(ggplot2)


###producing the covariance matrix
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

##producing random bivariate draws
library(mvtnorm)
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

##function for producing the plot and saving it in pdf
plot.pdf <- function(Df){
  pdf("dataPlot.pdf", width=4, height=4.5)
  a <- ggplot(data = Df, 
         aes(x = X1, y = X2, colour= categ, fill=categ)) + 
    geom_point() +
    xlab("X1") +
    ylab("X2") +
    theme_bw() +
    theme(text=element_text(family="Arial"))
  print(a)
  dev.off()
}




#font_import(), uncomment if required

#main function
#the double indexes 00,01,10,11 model two subcategories for each of the two categories.
#Final outputs of the function are four different clusters of points.
Data <- function(no00, no01, no10, no11, mu00, mu01, mu10, mu11, sd00, 
                     sd01, sd10, sd11, rho00, rho01, rho10, rho11, seed=1111, save = TRUE) {
  sigma00 <- sigmaXY(rho=rho00, sdX=sd00[1], sdY=sd00[2])
  sigma01 <- sigmaXY(rho=rho01, sdX=sd01[1], sdY=sd01[2]) 
  sigma10 <- sigmaXY(rho=rho10, sdX=sd10[1], sdY=sd10[2])
  sigma11 <- sigmaXY(rho=rho11, sdX=sd11[1], sdY=sd11[2])
  c00 <- genBVN(no00, mu00, sigma00, seed = seed)
  c01 <- genBVN(no01, mu01, sigma01, seed = seed+1)
  c10 <- genBVN(no10, mu10, sigma10, seed = seed+2)
  c11 <- genBVN(no11, mu11, sigma11, seed = seed+3)
  c0 <- rbind(c00,c01)
  c1 <- rbind(c10,c11)
  Df <- as.data.frame(rbind(c0,c1))
  categ <- as.matrix(c(rep("cat0", no00+ no01), rep("cat1", no10 + no11)))
  target <- as.matrix(c(rep(0, no00 + no01), rep(1, no10 + no11)))
  Df <- as.data.frame(cbind(Df, categ, target))
  colnames(Df) <- c("X1", "X2", "categ", "target")
  if (save == TRUE) {
    #loadfonts() uncomment if required
    plot.pdf(Df)
    write.csv(Df, file = "dataset.csv")
  }
 else {
   a <- ggplot(data = Df, 
          aes(x = X1, y = X2, colour= categ, fill=categ)) + 
     geom_point() +
     xlab("X1") +
     ylab("X2") +
     theme_bw() +
     theme(text=element_text(family="Arial"))
   plot(a)
 }  
 return(Df)
}

example <- Data(25, 25, 25, 25, c(1,20), c(20,1), c(1,1), c(20,20), c(2,2), c(2,2), c(2,2), c(2,2), 0, 0, 0, 0)

