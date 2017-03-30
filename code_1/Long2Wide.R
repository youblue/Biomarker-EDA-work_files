## Long- to wide-format data
Long2Wide <- function(biomarker.data){
    library(reshape2)
    data.wide <- dcast(biomarker.data,
                       SUBJECT + MARKER.NAME ~ WEEK,
                       value.var="MARKER.VALUE")
    
    return(data.wide)
}