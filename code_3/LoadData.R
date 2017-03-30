#'
#' A function for loading bayes data
#'
 
LoadData <- function(data.path) {
    cat("######=============================================######", "\n")
    cat("#####             0: Start loading data             #####", "\n")
    cat("######=============================================######", "\n")
    
    rm(list = ls())
    
    ## Locate path of Bayes data
    bayes.data.path <- file.path(data.path, "BayesDat.csv")
    
    ## Read in Bayes data
    bayes.data <- read.csv(bayes.data.path)
    
    ## Omit observations with NAs, if any
    bayes.data <- na.omit(bayes.data)
    
    ## Return
    return(bayes.data)
}
