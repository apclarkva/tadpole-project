#inputTrainingData <- read.csv("proposal/TADPOLE_InputData.csv")

# targetDataTrain <- read.csv("proposal/TADPOLE_TargetData_train.csv")
# 
targetDataValidation <- read.csv("proposal/TADPOLE_TargetData_validation_pre_processed.csv")
# 
# targetDataTest <- read.csv("proposal/TADPOLE_PredictTargetData_test.csv")
# 
# targetBaseline <- data.frame(names(targetDataValidation))
# 
# targetBaseline$Date <- targetDataValidation$Date
# targetBaseline$PTID_Key <- targetDataValidation$PTID_Key

## Create empty data frame
dfPtidDates <- function(dataSet){
  newDataSet = dataSet
  newDataSet[,3:8] <- NA
  return(newDataSet)
}

recentNonNAN <- function(v){
  NonNAindex <- which(!is.na(v))
  firstNonNA <- min(NonNAindex)
  v[firstNonNA]
}

fillFrame <- function(emptyValidation, exams, index){
  
  if (index == 160) {
     print("hello")
  }
  
  # Find PTID
  ptid <- exams$PTID_Key[1]
  # Find most recent dx change
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
  # Find most recent Ventricles
  ventricles <- recentNonNAN(exams$Ventricles)
  
  # Find most recent whole brain
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
  print(index)
}



