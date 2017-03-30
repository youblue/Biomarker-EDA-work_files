## Plot biomarker change before and after AE for each subject having AE
PlotBiomarkerChange.ae.subject <- function(data.fc, ae.times,
                                           ae.before.rows, ae.after.rows) {
    cat("######==============================================================######", "\n")
    cat("#####  5: Plot biomarker before and after AE for subjects having AE  #####", "\n")
    cat("######==============================================================######", "\n")
    
    ae.week.vec <- levels(factor(ae.times$EVENT.TIME) ) # Obtain the weeks of last treatment
    
    for(i in 1:length(ae.week.vec) ) {
        ae.week <- as.numeric(ae.week.vec[i])
        ae.subject <- ae.times$ID[ae.times$EVENT.TIME == ae.week] 
        
        ## Plot for subjects before and after AE,
        #  subjects with same treatment termination time are put together
        data.fc.ae <- data.fc[c(ae.before.rows, ae.after.rows), ]
        data.fc.ae <- data.fc.ae[which(data.fc.ae$SUBJECT %in% ae.subject), ]
        ggplot(data.fc.ae, aes(x = WEEK, y = MARKER.VALUE,
                               colour = factor(SUBJECT), group = SUBJECT ) ) +
            geom_line(aes(group = SUBJECT), size = 1) +
            geom_point(size = 2, shape = 21, fill = "white") +
            theme(legend.title = element_text() ) +
            scale_color_discrete(name = "SUBJECT") +
            facet_wrap(~ MARKER.NAME) +
            geom_vline(aes(xintercept = ae.week + 1),
                       linetype = 4, colour="black")
        
        save.path <- paste("./result_1/ChangeSubject",
                           ae.subject[1], "-", ae.subject[length(ae.subject)],
                           ".png", sep = "")
        ggsave(file = save.path, width = 12, height = 8)
    }
}