PredictAE <- function(data.ae.nae, lag = 3) {
    cat("######=============================================######", "\n")
    cat("#####    8: Predict AE at least one week in prior   #####", "\n")
    cat("######=============================================######", "\n")
    
    library(reshape2)
    library(e1071)
    # Row: Subject
    # Column: SUBJECT, AE,
    #         M1_1: marker 1 and week 1 | M3_1: marker 3 and week 1 | M4_1: marker 4 and week 1
    #         M1_2: marker 1 and week 2 | M3_2: marker 3 and week 2 | M4_2: marker 4 and week 2
    #         M1_3: marker 1 and week 3 | M3_3: marker 3 and week 3 | M4_3: marker 4 and week 3
    #         M1_4: marker 1 and week 4 | M3_4: marker 3 and week 4 | M4_4: marker 4 and week 4
    #         M1_5: marker 1 and week 5 | M3_5: marker 3 and week 5 | M4_5: marker 4 and week 5
    #         M1_6: marker 1 and week 6 | M3_6: marker 3 and week 6 | M4_6: marker 4 and week 6
    #         M1_7: marker 1 and week 7 | M3_7: marker 3 and week 7 | M4_7: marker 4 and week 7
    #         M1_8: marker 1 and week 8 | M3_8: marker 3 and week 8 | M4_8: marker 4 and week 8
    # NA demonstrates the data is missing (AE aready occurred before)
    #               
    data.all <- dcast(data.ae.nae, SUBJECT + AE ~ MARKER.NAME + WEEK, value.var="MARKER.VALUE")
    
    marker.list <- c("M1", "M3", "M4")
    
    data.feature <- NULL
    marker.week.featureMatrix <- NULL
    for(i in data.all$SUBJECT) {
        ae.week <- sum(is.na(data.all[i, -c(1,2)]) == FALSE) / length(marker.list)
        
        marker.week.feature <- NULL
        for(m in marker.list) {
            marker.week.feature <- c(marker.week.feature,
                                     paste(m, seq(ae.week-lag, ae.week-1), sep = "_") )
        }
        data.feature <- rbind(data.feature, as.matrix(data.all[i, marker.week.feature]) )
        marker.week.featureMatrix <- rbind(marker.week.featureMatrix, marker.week.feature)
    }
    
    data.feature <- data.frame(AE = data.all[,2], data.feature)
    write.csv(marker.week.featureMatrix,
              paste("./result_1/marker.week.featureMatrix_lag=", lag, ".csv", sep = "") )
    
    
    ## Divide the dataset into a train and a test set, for 3-fold cross-validation
    index.ae <- which(data.feature$AE == "YES")
    index.nae <- which(data.feature$AE == "NO")
    
    set.seed(1234)
    testindex.ae <- sample(index.ae, trunc(length(index.ae)/3) )
    testindex.nae <- sample(index.nae, trunc(length(index.nae)/3) )
    testindex <- sort(c(testindex.ae, testindex.nae) )
    
    testset <- data.feature[testindex, ]
    trainset <- data.feature[-testindex, ]
    
    ## Tune parameters
    tuned <- tune.svm(AE~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1) )
    summary(tuned)
    
    svmfit <- tuned$best.model
    
    ## Contingency table of the train set
    train.table <- table(trainset[,1], predict(svmfit) )
    print(train.contingency) # Print contingency table of the 
    
    
    
    ## Run the model again using the trained model and predict classes using the test set
    predict.table <- table(testset[, 1], predict(svmfit, testset[,-1]))
    
    ## Contingency table of the train set
    print(test.contigency)
    
    
    result.list <- list(train.table, predict.table)
    
    ## Return
    return(result.list)
    
}












