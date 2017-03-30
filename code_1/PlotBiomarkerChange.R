# Plot for each biomarker changes with weeks
PlotBiomarkerChange <- function(data.fc, ae.times) {
    cat("######===========================================================######", "\n")
    cat("#####    2: Plot biomarker change for all treated observations    #####", "\n")
    cat("######===========================================================######", "\n")
    
    source("./code_1/ExcludeAE.R")
    ## Exclude observations after reported AE
    result.list <- ExcludeAE(data.fc, ae.times)
    ae.before.rows <- result.list[[1]]
    ae.after.rows <- result.list[[2]]
    
    ## 6520 observtions excluding those after AE occurs
    data.fc.ec <- data.fc[-unique(ae.after.rows), ]
    
    ## Plot
    library(ggplot2)
    # Plot boxplot change of the 6520 observations
    ggplot(data.fc.ec, aes(x = WEEK,
                           y = MARKER.VALUE, fill = WEEK) ) +
        geom_boxplot() +
        facet_wrap(~ MARKER.NAME) +
        coord_cartesian(ylim=c(0,5) )
    ggsave(file = "./result_1/biomarker-change.png",
           width = 12, height = 8)
    
    ## Return
    return(list(ae.before.rows, ae.after.rows) )
}


