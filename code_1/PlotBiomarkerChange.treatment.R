## Plot which biomarkers are changing as a result of treatment
PlotBiomarkerChange.treatment <- function(data.fc, ae.before.rows, ae.after.rows){
    cat("######============================================================######", "\n")
    cat("#####  3: Plot biomarker for treated vs. non-treated observations  #####", "\n")
    cat("######============================================================######", "\n")
    
    source("./code_1/GroupByTreatment.R")
    
    ## Group observations of AE subjects by non-treatment and treatment
    result.list <- GroupByTreatment(data.fc, ae.before.rows, ae.after.rows)
    non.treatment.rows <- result.list[[1]]
    treatment.rows <- result.list[[2]]
    
    data.fc.nontreatment <- data.frame(data.fc[non.treatment.rows, ],
                                       TREATMENT = c(rep("NO", length(non.treatment.rows) ) ) )
    data.fc.treatment <- data.frame(data.fc[treatment.rows, ],
                                    TREATMENT = c(rep("YES", length(treatment.rows) ) ) )
    data.fc <- rbind(data.fc.nontreatment, data.fc.treatment)
        
    ## Plot
    library(ggplot2)
    require(Hmisc)
    # Plot mean(std) fold change of the AE subjects before and after treatment
    ggplot(data.fc, aes(x = WEEK,
                        y = MARKER.VALUE, colour = TREATMENT ) ) +
        stat_summary(fun.y = mean, geom="point") +
        stat_summary(fun.y = mean, geom = "line", aes(group = TREATMENT)) +
        stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
    
        facet_wrap(~ MARKER.NAME) +
        coord_cartesian(ylim=c(0,5) )
    
    ggsave(file = "./result_1/biomarker-change-treatment.png",
           width = 12, height = 8)
    
    ## Return
    return(list(non.treatment.rows, treatment.rows) )
}


