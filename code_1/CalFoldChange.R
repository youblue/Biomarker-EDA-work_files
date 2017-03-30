#' A function for calculating fold change
#' 
#' Columns are: 1:  MARKER.NAME
#'              2:  SUBJECT
#'              3:  Week0
#'              4:  Week1
#'              5:  Week2
#'              6:  Week3
#'              7:  Week4
#'              8:  Week5
#'              9:  Week6
#'              10: Week7
#'              11: Week8
#'

CalFoldChange <- function(data.wide){
    data.wide.fc <- data.wide
    for(i in 1:nrow(data.wide.fc) ){
        for(j in 11:3){
            if(data.wide.fc[i,3] == 0 & data.wide.fc[i,j] == 0){
                data.wide.fc[i,j] <- 1
            } else{
                data.wide.fc[i,j] <- data.wide.fc[i,j]/data.wide.fc[i,3]
            }
            
        }
    }
    return(data.wide.fc)
}








