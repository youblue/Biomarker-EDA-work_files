PlotCV <- function(data, mean, post.beta1, post.beta2) {
    cat("######=============================================######", "\n")
    cat("#####      2: Plot raw data and the fitted %CV      #####", "\n")
    cat("######=============================================######", "\n")
    
    source("./code_3/CV.R")
    
    attach(data)
    
    ## Calculate the coefficient of variation by raw data
    cv <- rep(0, nrow(data) )
    std <- rep(0, nrow(data) )
    for(i in 1:nrow(data) ) {
        std[i] <- sd(c(Y1[i], Y2[i]) )
        cv[i] <- CV(mean[i], std[i])
    }
    
    ## Plot the coefficient of variation by raw data
    png("./result_3/replicate_cv.png", width = 700, height = 400)
    plot(mean, cv, xlim = c(0,10), ylim = c(0, 150), col = "aquamarine4",
         xlab = "mu", ylab = "replicate %CV", cex = 1.5, cex.lab = 1.5)
    
    ## Add the fitted line
    mu <- log(mean)
    sigma <- post.beta1 / exp(post.beta2 * mu)
    cv.fitted <- 100 * sqrt(exp(sigma^2) - 1)
    lines(mean, cv.fitted, xlim = c(0,10), ylim = c(0, 150), type = "l")
    
    dev.off()
    
}