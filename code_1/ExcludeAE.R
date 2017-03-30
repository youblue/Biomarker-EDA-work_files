## Exclude the observations that are without treatment (after AE occurs)
ExcludeAE <- function(data.fc, ae.times) {
    ae.before.rows <- NULL
    ae.after.rows <- NULL
    
    for(i in 1:nrow(ae.times) ) {
        ae.before.index <- which(data.fc$SUBJECT == ae.times[i,1] &
                                     as.numeric(data.fc$WEEK) - 1 <= as.numeric(ae.times[i,2]) )
        
        ae.after.index <- which(data.fc$SUBJECT == ae.times[i,1] &
                                    as.numeric(data.fc$WEEK) - 1 > as.numeric(ae.times[i,2]) )
        
        ## rows of AE observations before AE occurs
        ae.before.rows <- c(ae.before.rows, ae.before.index)
        ## rows of AE observations after AE occurs
        ae.after.rows <- c(ae.after.rows, ae.after.index)
    }
    
    ae.before.rows <- unique(ae.before.rows)
    ae.after.rows <- unique(ae.after.rows)
    
    return(list(ae.before.rows, ae.after.rows) )
}