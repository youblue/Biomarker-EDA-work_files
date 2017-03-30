#'##############################################################################
#' Main script, can directly run
#' 
#' @description 
#' This script is responsible for calling functions:
#' 
#' 0. LoadData(): Load biomarker and adverse event data
#' 1. PreprocessData(): Formatting data, calculate fold change
#' 2. PlotBiomarkerChange(): Plot all biomarker change by week
#'                           for all treated observations
#' 3. PlotBiomarkerChange.treatment(): Biomarker value comparison
#'                                     between NON-treatment and treatment
#'                                     group of AE subjects
#' 4. Test.treatment(): Test if markers are related to treatment
#' 5. PlotBiomarkerChange.ae.subject(): Biomarker change before and after AE
#'                                      for each AE subject
#' 6. PlotBiomarkerChange.ae(): Biomarker comparison between NON-AE and AE group
#' 7. Test.ae(): Test if mean difference in NON-AE and AE subjects is 0
#' 8. PredictAE(): Predict AE at least one week in prior
#' 
#' @param
#' biomarker.data: original biomarker data
#' ae.times: original adverse event data
#' data.wide: wide-format of original data
#' data.wide.fc: wide-format fold change data
#' data.fc: long-format fold change data
#' ae.before.rows: row index before AE occurs for the AE subjects
#' ae.after.rows: row index after AE occurs for the AE subjects
#' non.treatment.rows: same as as.after.rows 
#' treatment.rows: ae.before.rows excluding those for WEEK0
#' data.fc.ae: Fold change of AE treated subjects only for M1, M3, M4
#' data.fc.nae: Fold change of NAE subjects only for M1, M3, M4
#' data.ae.nae: combination of data.fc.ae and data.fc.nae
#' train.contingency: Coningency table for train set
#' test.contingency: Coningency table for test set
#' result.list: temporarily save function return values
#' 
#'##############################################################################


# Set the home dir
setwd("E:/gilead")

source("./code_1/LoadData.R")
source("./code_1/PreprocessData.R")
source("./code_1/PlotBiomarkerChange.R")
source("./code_1/PlotBiomarkerChange.treatment.R")
source("./code_1/Test.treatment.R")
source("./code_1/PlotBiomarkerChange.ae.subject.R")
source("./code_1/PlotBiomarkerChange.ae.R")
source("./code_1/Test.ae.R")
source("./code_1/PredictAE.R")

## 0. Load Data
data.path = "./data"
result.list <- LoadData(data.path)
biomarker.data <- result.list[[1]]
ae.times <- result.list[[2]]

## 1. Preprocess Data
result.list <- PreprocessData(biomarker.data)
data.wide <- result.list[[1]]
data.wide.fc <- result.list[[2]]
data.fc <- result.list[[3]]

######## Question 1 ########

## 2. Plot for each biomarker changes with weeks, for all observations before AE occurs
result.list <- PlotBiomarkerChange(data.fc, ae.times)
ae.before.rows <- result.list[[1]]
ae.after.rows <- result.list[[2]]

## 3. Biomarker value comparison between NON-treatment and treatment group of AE subject
result.list <- PlotBiomarkerChange.treatment(data.fc, ae.before.rows, ae.after.rows)
non.treatment.rows <- result.list[[1]]
treatment.rows <- result.list[[2]]

## 4. Test if markers are related to treatment, for AE subject
result.list <- Test.treatment(data.fc, ae.times, non.treatment.rows, treatment.rows)

## 5. Biomarker change before and after AE, for subjects having AE
result.list <- PlotBiomarkerChange.ae.subject(data.fc, ae.times,
                                              ae.before.rows, ae.after.rows)

######## Question 2 ########

## 6. Biomarker value comparison between NON-AE group and AE group
result.list <- PlotBiomarkerChange.ae(data.fc, ae.times, ae.before.rows)
data.fc.ae <- result.list[[1]]
data.fc.nae <- result.list[[2]]

## 7. Test if mean difference in NON-AE and AE subjects is 0
result.list <- Test.ae(data.fc.ae, data.fc.nae)

######## Question 3 ########

## 8. Predict AE at least one week in prior
data.fc.ae <- data.fc.ae[which(data.fc.ae$MARKER.NAME != "M2"), ]
data.fc.nae <- data.fc.nae[which(data.fc.nae$MARKER.NAME != "M2"), ]
data.ae.nae <- rbind(data.fc.ae, data.fc.nae)

result.list <- PredictAE(data.ae.nae, 3)
train.contingency <- result.list[[1]] # Coningency table for train set
test.contingency <- result.list[[2]] # Coningency table for test set



