RunWinbugs <- function(data, winbugs.dir = "c:/Program Files/WinBUGS14/",
                       model.file = "E:/gilead/code_3/model.bug",
                       n.chains = 1, n.iter = 1200, n.burnin = 200, n.thin = 1) {
    cat("######============================================######", "\n")
    cat("#####           1: Run Winbugs with:               #####", "\n")
    cat("#####              n.chains (default = 1),         #####", "\n")
    cat("#####              n.iter (default = 1200),        #####", "\n")
    cat("#####              n.burnin (default = 200),       #####", "\n")
    cat("#####              n.thin (default = 1)            #####", "\n")
    cat("######============================================######", "\n")
    
    library(R2WinBUGS)
    attach(data)
    
    ## Prior for beta1 and beta2
    set.seed(1234)
    a <- 0.2
    s <- 0.001
    beta1 <- rgamma(n = 1, shape = a, scale = 1/s)
    beta2 <- rgamma(n = 1, shape = a, scale = 1/s)
    
    ## mu and sigma
    mu <- log(mean)
    sigma <- beta1 / exp(beta2*mu)
    
    ## Run MCMC
    N <- length(Y1)
    data <- list("Y1","Y2","N","mu")
    beta1 = rgamma(1,a,1/s)
    beta2 = rgamma(1,a,1/s)
    inits <- function(){
        list(beta1 = 0.1, beta2 = 0.1)
    }
    sim = bugs(data = data,inits = inits,
               model.file = model.file,
               parameters = c("beta1","beta2"),
               n.chains = n.chains, n.iter = n.iter,
               n.burnin = n.burnin, n.thin = n.thin,
               bugs.directory = winbugs.dir)
    
    
    ## Plot traces
    png("./result_3/ts_plot.png", width = 1400, height = 400)
    
    par(mfrow = c(1,2))
    
    ts.plot(sim$sims.array[,1,1], xlab = "iterations", ylab = "", main = "Trace plot: beta1")
    ts.plot(sim$sims.array[,1,2], xlab = "iterations", ylab = "", main = "Trace plot: beta2")
    
    par(mfrow = c(1,1))
    
    dev.off()
    
    ## Return
    return(sim)
}






