trainingIn <- read.csv("data/trainingInWithDays.csv")
validationIn <- read.csv("data/validationInWithDays.csv")

trainingOut <- read.csv("data/trainingOutWithDays.csv")
validationOut <- read.csv("data/validationOutWithDays.csv")

# I want to find the most common 
baseDxChange = tail(names(sort(table(trainingIn$DXCHANG))), 1)
meanADAS13 = mean(trainingIn$ADAS13, na.rm = TRUE)
meanVV = mean(trainingIn$Ventricular_Volume, na.rm = TRUE)
meanBV = mean(trainingIn$Brain_Volume, na.rm = TRUE)
meanMMSE = mean(trainingIn$MMSE, na.rm = TRUE)
meanAge = mean(trainingIn$AGE, na.rm = TRUE)
baseAPOE  = tail(names(sort(table(trainingIn$APOE))), 1)

#######
# Find indices of missing values

fillMissing <- function(exams){
  setDT(exams)[,ADAS13 := na.locf(na.locf(ADAS13, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,DXCHANG := na.locf(na.locf(DXCHANG, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,Ventricular_Volume := na.locf(na.locf(Ventricular_Volume, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,Brain_Volume := na.locf(na.locf(Brain_Volume, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,MMSE := na.locf(na.locf(MMSE, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,AGE := na.locf(na.locf(AGE, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]
  setDT(exams)[,SEX := na.locf(na.locf(SEX, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,APOE := na.locf(na.locf(APOE, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]
  
  
  exams$DXCHANG[is.na(exams$DXCHANG)] <- baseDxChange
  exams$ADAS13[is.na(exams$ADAS13)] <- meanADAS13
  exams$Ventricular_Volume[is.na(exams$Ventricular_Volume)] <- meanBV
  exams$Brain_Volume[is.na(exams$Brain_Volume)] <- meanBV
  exams$MMSE[is.na(exams$MMSE)] <- meanMMSE
  exams$APOE[is.na(exams$APOE)] <- baseAPOE
  
  return(exams)
}


trainingIn <- fillMissing(trainingIn)
validationIn <- fillMissing(validationIn)


fillMissingOut <- function(exams){
  setDT(exams)[,ADAS13 := na.locf(na.locf(ADAS13, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,Ventricles_Norm := na.locf(na.locf(Ventricles_Norm, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,MMSE := na.locf(na.locf(MMSE, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  
  setDT(exams)[,CN_Diag := na.locf(na.locf(CN_Diag, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]
  setDT(exams)[,MCI_Diag := na.locf(na.locf(MCI_Diag, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]  
  setDT(exams)[,AD_Diag := na.locf(na.locf(AD_Diag, na.rm=FALSE), fromLast=TRUE) , by = PTID_Key]
  
  
  exams$ADAS13[is.na(exams$ADAS13)] <- meanADAS13
  exams$Ventricles_Norm[is.na(exams$Ventricles_Norm)] <- meanVV/meanBV
  exams$MMSE[is.na(exams$MMSE)] <- meanMMSE
  return(exams)
}

trainingOut <- fillMissingOut(trainingOut)
validationOut <- fillMissingOut(validationOut)

write.csv(trainingIn, file ="data/trainingInFinal.csv")
write.csv(trainingOut, file="data/trainingOutFinal.csv")

write.csv(validationIn, file="data/validationInFinal.csv")
write.csv(validationOut, file="data/validationOutFinal.csv")