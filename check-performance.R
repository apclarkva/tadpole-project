## Calculate Diagnostic precision (validationToFill AND targetDataValidation)
diagnosticCount = c() # Save matches as 1 and non-matches as 0
adasPercent = c()
ventricularVolPercent = c()
mmsePercent = c()


for (index in c(1:dim(validationToFill)[1])){
  estimate <- validationToFill[index,]
  real <- targetDataValidation[index,]
  
  if(index == 116 || index == 117) {
    print(index)
  }
  
  if(!is.na(real$CN_Diag) && !is.na(estimate$CN_Diag)) {
    if(estimate$CN_Diag == real$CN_Diag & estimate$MCI_Diag == real$MCI_Diag & estimate$AD_Diag == real$AD_Diag){
      diagnosticCount = c(diagnosticCount, 1)
    } else {
      diagnosticCount = c(diagnosticCount, 0)
    }
  }
  
  if(!is.na(real$ADAS13) && !is.na(estimate$ADAS13)) {
    percentDifference = abs(real$ADAS13 - estimate$ADAS13) 
    adasPercent = c(adasPercent, percentDifference)
  }
  
  if(!is.na(real$Ventricles_Norm) && !is.na(estimate$Ventricles_Norm)) {
    percentDifference = abs(real$Ventricles_Norm - estimate$Ventricles_Norm) / real$Ventricles_Norm
    ventricularVolPercent = c(ventricularVolPercent, percentDifference)
  }
  
  if(!is.na(real$MMSE) && !is.na(estimate$MMSE)) {
    percentDifference = abs(real$MMSE - estimate$MMSE)
    mmsePercent = c(mmsePercent, percentDifference)
  }
}



diagnosticPerformance = length(which(diagnosticCount >.5)) / length(diagnosticCount)


