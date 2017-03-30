Test.ae <- function(data.fc.ae, data.fc.nae) {
    cat("######=============================================######", "\n")
    cat("#####  7: T-test between NON-AE group and NE group  #####", "\n")
    cat("######=============================================######", "\n")
    
    levels.ae.subject <- as.numeric(levels(factor(data.fc.ae$SUBJECT) ) ) # All AE subjects
    levels.ae.week <- as.numeric(levels(factor(data.fc.ae$WEEK) ) ) # All AE weeks
    levels.ae.markername <- levels(factor(data.fc.ae$MARKER.NAME) ) # All AE marker names
    
    levels.nae.subject <- as.numeric(levels(factor(data.fc.nae$SUBJECT) ) ) # All NAE subjects
    levels.nae.week <- as.numeric(levels(factor(data.fc.nae$WEEK) ) ) # All NAE weeks
    levels.nae.markername <- levels(factor(data.fc.nae$MARKER.NAME) ) # All NAE marker names
    
    # Only consider the overlapping markers and weeks to do test
    levels.markername <- intersect(levels.ae.markername, levels.nae.markername) 
    levels.week <- intersect(levels.ae.week, levels.nae.week)
    
    ## Mean (Standard deviation) of AE and NAE subjects, also
    #  calculate p-value if the difference of groupo mean is not equal to 0
    p.value.ae <- NULL
    mean.ae <- NULL # Mean of the AE subjects
    std.ae <- NULL # Standard deviation of the AE subjects
    mean.nae <- NULL # Mean of the NAE subjects
    std.nae <- NULL # Standard deviation of the NAE subjects
    
    for(marker.name in levels.markername) { # For each marker of M1, M2, M3, M4
        mean.ae.value <- NULL # Mean of the AE subjects for current marker
        std.ae.value <- NULL # Standard deviation of the AE subjects for current marker
        mean.nae.value <- NULL # Mean of the NAE subjects  for current marker
        std.nae.value <- NULL # Standard deviation of the NAE subjects for current marker
        
        for(week in levels.week) { # For each of the overlapping week for AE and NAE
            ae.value <- data.fc.ae[which(data.fc.ae$MARKER.NAME == marker.name &
                                             data.fc.ae$WEEK == week), "MARKER.VALUE"]
            nae.value <- data.fc.nae[which(data.fc.nae$MARKER.NAME == marker.name &
                                             data.fc.nae$WEEK == week), "MARKER.VALUE"]
            mean.ae.value <- c(mean.ae.value, mean(ae.value) )
            std.ae.value <- c(std.ae.value, sd(ae.value) )
            mean.nae.value <- c(mean.nae.value, mean(nae.value) )
            std.nae.value <- c(std.nae.value, sd(nae.value) )
        }
        p.value.ae <- c(p.value.ae, t.test(
            mean.ae.value - mean.nae.value, c(rep(0, length(mean.ae.value) ) ) )$p.value)
        
        mean.ae <- c(mean.ae, mean.ae.value)
        std.ae <- c(std.ae, std.ae.value)
        mean.nae <- c(mean.nae, mean.nae.value)
        std.nae <- c(std.nae, std.nae.value)
        
    }
    
    result.list <- data.frame(p.value.ae, mean.ae, std.ae, mean.nae, std.nae)
    write.csv(result.list, "./result_1/Result_ttest_ae.csv")
    
    return(result.list)
}



