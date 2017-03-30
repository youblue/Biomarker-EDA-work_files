#'####################################################################################################################
#' Main script, can directly run
#' 
#' This script is responsible for calling functions:
#' 0. LoadData(): Load Bayes data
#' 1. RunWinbugs(): Run WinBUGS to obtain posterior mean of beta1 and beta2
#' 2. PlotCV(): Plot raw data and the fitted %CV
#' 3. PostBeta1(), PostBeta2(): Posterior of beta1/beta2 when beta2/beta1 is known
#'    LogPostBeta1(), LogPostBeta2(): Log-Posterior of beta1/beta2 when beta2/beta1 is known
#'    dLogPostBeta1(), dLogPostBeta2(): First derivative of Log-Posterior of beta1/beta2 when beta2/beta1 is known
#'    ddLogPostBeta1(), ddLogPostBeta2(): Second derivative of Log-Posterior of beta1/beta2 when beta2/beta1 is known
#'####################################################################################################################


# Set the home dir
setwd("E:/gilead")
# Paths of functions
source("./code_3/LoadData.R")
source("./code_3/RunWinbugs.R")
source("./code_3/PlotCV.R")
source("./code_3/CalPosterior.R")

## 0. Load Data
data.path = "./data"
data <- LoadData(data.path)
attach(data)

## 1. Run WinBUGS to obtain posterior means of beta1 and beta2
sim <- RunWinbugs(data, "c:/Program Files/WinBUGS14/",
                  "E:/gilead/code_3/model.bug", 1, 1200, 200, 1)

post.beta1 <- sim$mean[[1]] # Posterior mean of beta1
post.beta2 <- sim$mean[[2]] # Posterior mean of beta2

print(sim) # Pring summery of the posteriors

## 2. Plot raw data and the fitted %CV
PlotCV(data, mean, post.beta1, post.beta2)


## 3. Optional: Try to plot the shape of conditional posterior distributions
## for Posterior, Log Posterior, First and Second Derivative of Log-Posterior
Z1 <- log(Y1) # For simplify, transform to normal
Z2 <- log(Y2) # For simplify, transform to normal

## Example: Plot posterior of beta1
beta1 <- seq(0.01,1,0.001)
post.beta1 <- LogPostBeta1(Z1, Z2, length(mean), mu, beta1, 0.65)
plot(beta1, post.beta1)

## Example: Plot posterior of beta2
beta2 <- seq(0.01,1,0.001)
post.beta2 <- c(rep(0,length(beta2)))
for(i in 1:length(beta2)){
    post.beta2[i] <- LogPostBeta2(Z1, Z2, length(mean), mu, 0.15, beta2[i])
}
plot(beta2, post.beta2)





