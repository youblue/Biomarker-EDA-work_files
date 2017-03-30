## Calculate Coefficient of Variance based on raw data
CV <- function(mean, sd) { # input mean, standard deviation of the data
    
    ## Return
    (sd / mean) * 100
}