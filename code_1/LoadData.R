LoadData <- function(data.path) {
    cat("######=============================================######", "\n")
    cat("#####             0: Start loading data             #####", "\n")
    cat("######=============================================######", "\n")
    
    rm(list = ls())
    ## Locate path of Biomarker and AE_times data
    biomarker.data.path <- file.path(data.path, "Biomarker Data.csv")
    ae.times.path <- file.path(data.path, "AE times.csv")
    
    ## Read in Biomarker and AE_times data
    biomarker.data <- read.csv(biomarker.data.path)
    ae.times <- read.csv(ae.times.path)
    
    ## Omit observations with NAs, if any
    biomarker.data <- na.omit(biomarker.data)
    ae.times <- na.omit(ae.times)
    
    ## Adjust the orders of levels for the "MARKER.NAME" column,
    ## labelled as "M0", "M1", "M2", ... , "M20" 
    marker.order <- paste("M", seq(1:20), sep = "")
    biomarker.data$MARKER.NAME <- ordered(biomarker.data$MARKER.NAME,
                                          levels = marker.order,
                                          labels = marker.order)
    
    ## Return
    return(list(biomarker.data, ae.times))
}

