PreprocessData <- function(biomarker.data, ae.times){
    cat("######=============================================######", "\n")
    cat("#####           1: Preprocess data format           #####", "\n")
    cat("######=============================================######", "\n")
    
    source("./code_1/Long2Wide.R")
    source("./code_1/Wide2Long.R")
    source("./code_1/CalFoldChange.R")
    
    ## Long- to wide-format data
    data.wide <- Long2Wide(biomarker.data)
    
    # Calculate fold change
    data.wide.fc <- CalFoldChange(data.wide)
    
    # melt back to long format again
    data.fc <- Wide2Long(data.wide.fc)
    
    return(list(data.wide, data.wide.fc, data.fc))
    
}