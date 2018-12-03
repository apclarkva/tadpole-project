library(dplyr)

#targetDataTest <- read.csv("proposal/TADPOLE_PredictTargetData_test.csv")

# This is the baseline script. In this script, I use the examinations from TADPOLE_InputData.csv to fill in predictions for the diagnosis, ADAS13, normalized ventricle size, and MMSE on future examination dates for patients. Most (possibly all) patients will have multiple future dates to predict.

inputTrainingData <- read.csv("proposal/TADPOLE_InputData.csv")

targetDataTrain <- read.csv("Alex/TADPOLE_TargetData_train_pre_processed.csv")

targetDataValidation <- read.csv("Alex/TADPOLE_TargetData_validation_pre_processed.csv")

## Create empty data frame
dfPtidDates <- function(dataSet){
  newDataSet = dataSet
  newDataSet[,3:8] <- NA
  return(newDataSet)
}

# Given a set of exams, find the most recent non-null value. Often, there will be empty fields in a set of exams. For the move-forward baseline method, we only care about the most-recent exam.
recentNonNAN <- function(v){
  NonNAindex <- which(!is.na(v))
  firstNonNA <- min(NonNAindex)
  v[firstNonNA]
}

fillFrame <- function(emptyValidation, exams, index){
  # Find PTID
  ptid <- exams$PTID_Key[1]
  
  # Find most recent dx change
  # Because dxChange is a measure of how someone's diagnosis has changed, we use it to fill in the dianostic values.
  dxChange <- recentNonNAN(exams$DXCHANGE)
  if ( dxChange %in% c(1,7,9)) {
    emptyValidation[emptyValidation$PTID_Key == ptid,]$CN_Diag = 1
    emptyValidation[emptyValidation$PTID_Key == ptid,]$MCI_Diag = 0 
    emptyValidation[emptyValidation$PTID_Key == ptid,]$AD_Diag = 0
  } else if ( dxChange %in% c(2,4,8)) {
    emptyValidation[emptyValidation$PTID_Key == ptid,]$CN_Diag = 0
    emptyValidation[emptyValidation$PTID_Key == ptid,]$MCI_Diag = 1 
    emptyValidation[emptyValidation$PTID_Key == ptid,]$AD_Diag = 0
  } else if (dxChange %in% c(3,5,6)) {
    emptyValidation[emptyValidation$PTID_Key == ptid,]$CN_Diag = 0
    emptyValidation[emptyValidation$PTID_Key == ptid,]$MCI_Diag = 0
    emptyValidation[emptyValidation$PTID_Key == ptid,]$AD_Diag = 1
  } else {
    emptyValidation[emptyValidation$PTID_Key == ptid,]$CN_Diag = NA
    emptyValidation[emptyValidation$PTID_Key == ptid,]$MCI_Diag = NA
    emptyValidation[emptyValidation$PTID_Key == ptid,]$AD_Diag = NA
  }
  
  # Find most recent ADAS13
  ADAS13 <- recentNonNAN(exams$ADAS13)
  emptyValidation[emptyValidation$PTID_Key == ptid,]$ADAS13 = ADAS13
  
  # Find most recent Ventricles, wholebrain, then normalize
  ventricles <- recentNonNAN(exams$Ventricles)
  wholeBrain <- recentNonNAN(exams$WholeBrain)
  emptyValidation[emptyValidation$PTID_Key == ptid,]$Ventricles_Norm = ventricles / wholeBrain
  
  # Find most recent MMSE
  MMSE <- recentNonNAN(exams$MMSE)
  emptyValidation[emptyValidation$PTID_Key == ptid,]$MMSE = MMSE
  
  return(emptyValidation)
}

# Steps for finding baseline
## Get all PTID keys in validation dataset
## Remove all NAN 
## DONE: Create an empty data frame with PTID and names from validation dataset
## Find the last date in the inputTrainingData with diagnosis (DX_change), ADAS13 (ADAS13), Ventricles_Norm ($Ventricles / $WholeBrain), MMSE (MMSE)
## Fill empty data frame with values from last visit date for each patient
## Compare to actual

validationToFill <- dfPtidDates(targetDataValidation)
patients <- unique(validationToFill$PTID)

for (index in c(1:length(patients))){
  
  currentPatientExams <- inputTrainingData[inputTrainingData$PTID_Key == patients[index],]
  currentPatientExams <- currentPatientExams[!is.na(currentPatientExams$PTID_Key), ]
  currentPatientExams[order(as.Date(as.character(currentPatientExams$EXAMDATE), format="%m/%d/%y"), decreasing = TRUE),]
  
  currentPatientExams <- currentPatientExams[nrow(currentPatientExams):1,]
  
  if(index == 160) {
    print("Hello")
  }
  
  validationToFill <- fillFrame(validationToFill, currentPatientExams, index)
}



# Find patients in training and validation datasets
patientsTraining <- unique(targetDataTrain$PTID)
patientsValidation <- unique(targetDataValidation$PTID)

# Find patient exams for training and 
patientExamsTraining <- inputTrainingData[inputTrainingData$PTID_Key %in%  patientsTraining,]
patientExamsValidation <- inputTrainingData[inputTrainingData$PTID_Key %in%  patientsValidation,]

# find inputs for the RNN
df <- patientExamsTraining
patientsTrainingIn <- data.frame(Date = df$EXAMDATE, PTID_Key = df$PTID_Key, DXCHANG = df$DXCHANGE, ADAS13 = df$ADAS13, Ventricular_Volume = df$Ventricles, Brain_Volume = df$WholeBrain, MMSE = df$MMSE, AGE = df$AGE, SEX = df$PTGENDER, APOE = df$APOE4)
write.csv(patientsTrainingIn, file = "Alex/data/trainingInputsRNN.csv")

# Find validation inputs for the RNN
df <- patientExamsValidation
patientsValidationIn <- data.frame(Date = df$EXAMDATE, PTID_Key = df$PTID_Key, DXCHANG = df$DXCHANGE, ADAS13 = df$ADAS13, Ventricular_Volume = df$Ventricles, Brain_Volume = df$WholeBrain, MMSE = df$MMSE, AGE = df$AGE, SEX = df$PTGENDER, APOE = df$APOE4)
write.csv(patientsValidationIn, file = "Alex/data/validationInputsRNN.csv")








## Junk Code
targetBaseline <- data.frame(matrix(nrow = dim(targetDataValidation)[1], ncol = dim(targetDataValidation)[2])) 

names(targetBaseline) <- names(targetDataValidation)

targetBaseline$Date <- targetDataValidation$Date
targetBaseline$PTID_Key <- targetDataValidation$PTID_Key