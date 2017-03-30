## Plot the change in which markers is related to the event
PlotBiomarkerChange.ae <- function(data.fc, ae.times, ae.before.rows) {
    cat("######=============================================================######", "\n")
    cat("#####        6: Plot biomarker values in AE and NAE subjects        #####", "\n")
    cat("######=============================================================######", "\n")
    
    ## For both AE and NAE subjects choose only the first four markers
    
    data.fc.ae <- data.fc[ae.before.rows, ]
    data.fc.ae <- data.fc.ae[which(data.fc.ae$SUBJECT %in% ae.times[,1] &
                              as.numeric(data.fc.ae$WEEK) - 1 != 0 &
                              data.fc.ae$MARKER.NAME %in% c("M1", "M2", "M3", "M4") ), ]
    
    data.fc.nae <- data.fc[which(!(data.fc$SUBJECT %in% ae.times[,1]) &
                                     as.numeric(data.fc$WEEK) - 1 != 0 &
                                     data.fc$MARKER.NAME %in% c("M1", "M2", "M3", "M4") ), ]
    
    data.fc.ae <- data.frame(data.fc.ae, AE = c(rep("YES", dim(data.fc.ae)[1] ) ) )
    data.fc.nae <- data.frame(data.fc.nae, AE = c(rep("NO", dim(data.fc.nae)[1] ) ) )
    data.fc <- rbind(data.fc.ae, data.fc.nae)
    
    ## Plot
    library(ggplot2)
    require(Hmisc)
    # Plot boxplot change of the 6520 observations
    ggplot(data.fc, aes(x = WEEK,
                        y = MARKER.VALUE, colour = AE ) ) +
        stat_summary(fun.y = mean, geom="point") +
        stat_summary(fun.y = mean, geom = "line", aes(group = AE)) +
        stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
        
        facet_wrap(~ MARKER.NAME)
    
    ggsave(file = "./result_1/biomarker-change-AE.png",
           width = 12, height = 8)
    
    ## Return
    return(list(data.fc.ae, data.fc.nae) )
}