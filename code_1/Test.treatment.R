Test.treatment <- function(data.fc, ae.times,
                            non.treatment.rows,
                            treatment.rows) {
    cat("######===========================================================######", "\n")
    cat("#####  4: T-test between NON-treatment group and treatment group  #####", "\n")
    cat("######===========================================================######", "\n")
    
    data.fc.nontreatment <- data.fc[non.treatment.rows, ]
    data.fc.treatment <- data.fc[treatment.rows, ]
    
    ae.fc.nontreatment <- NULL # Average fold change of AE subjects for nontreatment observations
    ae.fc.treatment <- NULL # Average fold change of AE subjects for treatment observations

    ## For each marker, first obtain average fold change across different week,
    #  for observations of AE subjections under treatment or without treatment
    marker.vec <- levels (data.fc$MARKER.NAME)
    for(i in 1:nrow(ae.times) ) {  # For each AE subject
        for(m in 1:length(marker.vec) ) { # For each biomarker
            
            ## For nontreatment observations
            nt <- data.fc.nontreatment [which(
                  data.fc.nontreatment$SUBJECT == ae.times[i,1] & # belongs to AE subjects
                  data.fc.nontreatment$MARKER.NAME == marker.vec[m] & # belongs to marker m
                  as.numeric(data.fc.nontreatment$WEEK) - 1 > ae.times[i,2]), ] # after AE occurs
            avg.nt <- nt[1, ] # If there's multiple observations, obtain the average
            avg.nt["MARKER.VALUE"] <- mean(nt$MARKER.VALUE)
            ae.fc.nontreatment <- rbind(ae.fc.nontreatment, avg.nt)
            
            ## For treatment observations
            t <- data.fc.treatment [which(
                 data.fc.treatment$SUBJECT == ae.times[i,1] & # belongs to AE subjects
                 data.fc.treatment$MARKER.NAME == marker.vec[m] & # belongs to marker m
                 as.numeric(data.fc.treatment$WEEK) - 1 <= ae.times[i,2]), ] # before AE occurs
            avg.t <- t[1, ] # If there's multiple observations, obtain the average
            avg.t["MARKER.VALUE"] <- mean(t$MARKER.VALUE)
            ae.fc.treatment <- rbind(ae.fc.treatment, avg.t)
        }
    }
    
    
    ## Calculate mean, standard deviation of the treatment and nontreatment group, also
    #  test if the two groups are difference, because for same subjects, I use paired t-test
    
    p.value.treatment <- c(rep(0, length(marker.vec) ) )
    mean.nontreatment <- c(rep(0, length(marker.vec) ) )
    std.nontreatment <- c(rep(0, length(marker.vec) ) )
    mean.treatment <- c(rep(0, length(marker.vec) ) )
    std.treatment <- c(rep(0, length(marker.vec) ) )
    
    for(m in 1:length(marker.vec) ) {
        marker.value.nontreatment <- ae.fc.nontreatment[
            which(ae.fc.nontreatment$MARKER.NAME ==
                      marker.vec[m]), "MARKER.VALUE"]
        marker.value.treatment <- ae.fc.treatment[
            which(ae.fc.treatment$MARKER.NAME ==
                      marker.vec[m]), "MARKER.VALUE"]
        
        # Check whether the mean of the values contained in marker.value.nontreatment
        # is less than the mean of the values contained in marker.value.treatment
        p.value.treatment[m] <- t.test(marker.value.nontreatment,
            marker.value.treatment, paired = TRUE, alt="less")$p.value
        
        mean.nontreatment[m] <- mean(marker.value.nontreatment)
        std.nontreatment[m] <- sd(marker.value.nontreatment)
        mean.treatment[m] <- mean(marker.value.treatment)
        std.treatment[m] <- sd(marker.value.treatment)
    }
    
    result.list <- data.frame(p.value.treatment, mean.nontreatment, std.nontreatment,
                              mean.treatment, std.treatment)
    
    write.csv(result.list, "./result_1/Result_ttest_treatment.csv")
    
    return(result.list)
}





