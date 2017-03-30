## Wide- to Long-format data
Wide2Long <- function(data.wide.fc){
    data.fc <- melt(data.wide.fc,
                    id.vars = c("SUBJECT", "MARKER.NAME"),
                    variable.name = "WEEK", 
                    value.name = "MARKER.VALUE")
    
    return(data.fc)
}