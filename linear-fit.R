#inputTrainingData <- read.csv("proposal/TADPOLE_InputData.csv")
#targetDataValidation <- read.csv("proposal/TADPOLE_TargetData_validation_pre_processed.csv")

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
  
  startDate = as.Date("1/10/07", format="%m/%d/%y")
  
  # Find PTID
  ptid <- exams$PTID_Key[1]
  
  # Patients to fill
  patientsToFill <- emptyValidation[emptyValidation$PTID_Key == ptid,]
  
  # Find most recent dx change
  examsWithDx <- exams[!is.na(exams$DXCHANGE),]
  
  examsWithDx$DXCHANGE[examsWithDx$DXCHANGE %in% c(1,7,9)] = 1
  examsWithDx$DXCHANGE[examsWithDx$DXCHANGE %in% c(2,4,8)] = 2
  examsWithDx$DXCHANGE[examsWithDx$DXCHANGE %in% c(3,5,6)] = 3
  
  examsWithDx$EXAMDATE <- as.integer(as.Date(as.character(examsWithDx$EXAMDATE), format="%m/%d/%y") - startDate)
  
  
  if(dim(examsWithDx)[1] > 0) {
    linearMod <- lm(DXCHANGE ~ EXAMDATE, data=examsWithDx)
    slope = linearMod$coefficients[2]
    intercept = linearMod$coefficients[1]
    if (is.na(slope)) {
      slope = 0
    }
    interceptADAS = linearMod$coefficients[1]
  } else {
    slopeADAS = NA
    interceptADAS = NA
  }
  
  
  
  
  
  
  ############
  # Find most recent ADAS13
  examsWithADAS = exams[!is.na(exams$ADAS13),]
  examsWithADAS$EXAMDATE <- as.integer(as.Date(as.character(examsWithADAS$EXAMDATE), format="%m/%d/%y") - startDate)
  
  if(dim(examsWithADAS)[1] > 0) {
    linearMod <- lm(ADAS13 ~ EXAMDATE, data=examsWithADAS)
    slopeADAS = linearMod$coefficients[2]
    if (is.na(slopeADAS)) {
      slopeADAS = 0
    }
    interceptADAS = linearMod$coefficients[1]
  } else {
    slopeADAS = NA
    interceptADAS = NA
  }
  
  
  # Find most recent Ventricles
  examsWithBrain = exams[(!is.na(exams$Ventricles)) & (!is.na(exams$WholeBrain)),]
  examsWithBrain$Ventricles = examsWithBrain$Ventricles / examsWithBrain$WholeBrain
  examsWithBrain$EXAMDATE <- as.integer(as.Date(as.character(examsWithBrain$EXAMDATE), format="%m/%d/%y") - startDate)
  
  if(dim(examsWithBrain)[1] > 0) {
    linearMod <- lm(Ventricles ~ EXAMDATE, data=examsWithBrain)
    slopeVentricles = linearMod$coefficients[2]
    if (is.na(slopeVentricles)) {
      slopeVentricles = 0
    }
    interceptVentricles = linearMod$coefficients[1]
  } else {
    slopeVentricles = NA
    interceptVentricles = NA
  }
  
  
  
  
  
  
  # Find most recent MMSE
  examsWithMMSE = exams[!is.na(exams$MMSE),]
  examsWithMMSE$EXAMDATE <- as.integer(as.Date(as.character(examsWithMMSE$EXAMDATE), format="%m/%d/%y") - startDate)
  
  if(dim(examsWithMMSE)[1] > 1) {
    linearMod <- lm(MMSE ~ EXAMDATE, data=examsWithMMSE)
    slopeMMSE = linearMod$coefficients[2]
    if(is.na(slopeMMSE)) {
      slopeMMSE = 0
    }
    interceptMMSE = linearMod$coefficients[1]
  } else {
    slopeMMSE = NA
    interceptMMSE = NA
  }
  
  
  
  
  
  ############
  dates = as.character(patientsToFill$Date)
  for(index in c(1:length(dates))) {
    futureExamDate = dates[index]
    days = as.integer(as.Date(futureExamDate, format="%m/%d/%y") - startDate)
    
    if (!is.na(slope)) {
      dxChange = days * slope + intercept
      
      if ( dxChange < 1.5) {
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$CN_Diag = 1
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$MCI_Diag = 0 
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$AD_Diag = 0
      } else  if (dxChange < 2.5 ){
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$CN_Diag = 0
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$MCI_Diag = 1 
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$AD_Diag = 0
      } else if (dxChange > 2.5 ){
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$CN_Diag = 0
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$MCI_Diag = 0
        emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$AD_Diag = 1
      } else {
        emptyValidation[emptyValidation$PTID_Key == ptid,]$CN_Diag = NA
        emptyValidation[emptyValidation$PTID_Key == ptid,]$MCI_Diag = NA
        emptyValidation[emptyValidation$PTID_Key == ptid,]$AD_Diag = NA
      }
    }
    
    if(!is.na(slopeADAS)){
      emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$ADAS13 = days * slopeADAS + interceptADAS
    }
    
    if(!is.na(slopeVentricles)) {
      emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$Ventricles_Norm = days * slopeVentricles + interceptVentricles
    }
    if(!is.na(slopeMMSE)){
      emptyValidation[emptyValidation$PTID_Key == ptid & emptyValidation$Date == futureExamDate,]$MMSE = days * slopeMMSE + interceptMMSE    
    }
    
  }
  
  

    

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



