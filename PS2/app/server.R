library(ggplot2)
library(mvtnorm)
library(shiny)
library(extrafont)

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
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}



shinyServer(function(input, output, session) {
  
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    select <- loanData(noApproved=50, noDenied=50, c(input$mean_PIapp, input$mean_solapp), c(input$mean_PIdeni, input$mean_soldeni), 
             c(input$sd_PIapp, input$sd_solapp), c(input$sd_PIdeni, input$sd_soldeni), -0.1, 0.6, 1221)
    return(select)
  })
  
  
  ##update regression line
  datafit <- reactive({
    modelfit <- lm(target ~ solvency + PIratio + 1, data=selectedData())
    weights <- coef(modelfit)[c("solvency", "PIratio")]
    bias <- coef(modelfit)[1]
    
    # Computing the boundary: since it is a 2-dimensional example the boundary 
    # is a line. 
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    predictedLabels <- ifelse(predict(modelfit) < 0.5, "Approved", "Denied")
    list(intercept = intercept, slope = slope, predictedLabels = predictedLabels)
  })
  
  #load fonts before plotting
  loadfonts()
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(data = selectedData(), aes(x = solvency, y = PIratio, 
                              colour=deny, fill=deny)) + 
      geom_point() +
      xlab("solvency") +
      ylab("PIratio") +
      theme_bw() +
      theme(text=element_text(family="Arial")) +
      geom_abline(intercept = datafit()$intercept, slope = datafit()$slope)
  })
  
  #plot of the contingency_matrix
  output$contingency_matrix <- renderTable({ 
    confMatrixFreq <- table(selectedData()$deny, datafit()$predictedLabels)
    confMatrixFreq
    })
})
