library(ggplot2)
library(mvtnorm)
# create small wrapper functions
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}


# creating a function for all of this
loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved, 
                     sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed=seed + 2)
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target = c(rep(0, noApproved), rep(1, noDenied), rep(2,noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}


# generating some data
loanDf <- loanData(noApproved=50, noDenied=50, noUndecided=50, c(4, 150), c(10, 100), c(4,100),  
                   c(1,20), c(2,30), c(1,15), -0.1, 0.6, 0.2, 1221)

# illustrating the data, note that with ggplot we need to additionally 
# specify font family
ggplot(data = loanDf, 
       aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial"))

###For computing the decision boundaries we first have to run three different regressions

##First regression between 



