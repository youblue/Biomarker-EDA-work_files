## Obtain row index of treatment and non-treatment for AE subjects
GroupByTreatment <- function(data.fc, ae.before.rows, ae.after.rows) {
    non.treatment.rows <- ae.after.rows
    treatment.rows <- ae.before.rows
    # Excluding WEEK0 observations
    treatment.rows <-
        treatment.rows[which(data.fc[ae.before.rows, ]$WEEK != 0)]
    
    ## Return
    return(list(non.treatment.rows, treatment.rows) )
}