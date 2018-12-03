## Load in training information
trainingInputsRNN <- read.csv("data/trainingInputsRNN.csv")
validationInputsRNN <- read.csv("data/validationInputsRNN.csv")

## Load in targets
trainingTargets <- read.csv("TADPOLE_TargetData_train_pre_processed.csv")
validationTargets <- read.csv("TADPOLE_TargetData_validation_pre_processed.csv")

           
countDays <- function(inputs, targets){
  # exams includes only the exams for one ptid
  # find the number of days between the current exam date, and the next date.
  
  inputs <- inputs[order(as.Date(as.character(inputs$Date), format="%m/%d/%y")),]
  targets <- targets[order(as.Date(as.character(targets$Date), format="%m/%d/%y")),]
  
  inputsLength = length(inputs$Date)
  targetsLength = length(targets$Date)
  numLoops = inputsLength + targetsLength - 1
  
  inputs$DaysUntilNext <- rep(0,inputsLength)
  targets$DaysUntilNext <- rep(0, targetsLength)
  
  for(index in c(1:numLoops)) {
    if(index == inputsLength) {
      currentDate = as.Date(as.character(inputs$Date[index]), format="%m/%d/%y")
      nextDate = as.Date(as.character(targets$Date[index - inputsLength+1]), format="%m/%d/%y")
      
      inputs$DaysUntilNext[index] = as.integer(nextDate - currentDate)
      
    } else if (index > inputsLength) {
      currentDate = as.Date(as.character(targets$Date[index - inputsLength]), format="%m/%d/%y")
      nextDate = as.Date(as.character(targets$Date[index+1-inputsLength]), format="%m/%d/%y")     
      
      targets$DaysUntilNext[index-inputsLength] = as.integer(nextDate - currentDate)
      
    } else {
      currentDate = as.Date(as.character(inputs$Date[index]), format="%m/%d/%y")
      nextDate = as.Date(as.character(inputs$Date[index+1]), format="%m/%d/%y")
      
      inputs$DaysUntilNext[index] = as.integer(nextDate - currentDate)
    }
  }
  
  return(list(inputs, targets))
}
           
newTrainingInRNN <- data.frame(matrix(ncol = dim(trainingInputsRNN)[2], nrow = 0))
names(newTrainingInRNN) <- names(trainingInputsRNN)
newTrainingOut <- data.frame(matrix(ncol = dim(trainingTargets)[2], nrow = 0))
names(newTrainingOut) <- names(trainingTargets)

newValidationInRNN <- data.frame(matrix(ncol = dim(validationInputsRNN)[2], nrow = 0))
names(newValidationInRNN) <- names(validationInputsRNN)
newValidationOut <- data.frame(matrix(ncol = dim(validationTargets)[2], nrow = 0))
names(newValidationOut) <- names(validationTargets)

for(ptid in unique(trainingInputsRNN$PTID_Key)) {
  newFrames = countDays(trainingInputsRNN[trainingInputsRNN$PTID_Key == ptid,], trainingTargets[trainingTargets$PTID_Key == ptid,])
  inputs <- as.data.frame(newFrames[1])
  targets <- as.data.frame(newFrames[2])
  
  newTrainingInRNN <- rbind(newTrainingInRNN, inputs)
  newTrainingOut <- rbind(newTrainingOut, targets)
}




for(ptid in unique(validationInputsRNN$PTID_Key)) {
  newFrames = countDays(validationInputsRNN[validationInputsRNN$PTID_Key == ptid,], validationTargets[validationTargets$PTID_Key == ptid,])
  inputs <- as.data.frame(newFrames[1])
  targets <- as.data.frame(newFrames[2])
  
  newValidationInRNN <- rbind(newValidationInRNN, inputs)
  newValidationOut <- rbind(newValidationOut, targets)
}

write.csv(newTrainingInRNN, file = "data/trainingInWithDays.csv")
write.csv(newTrainingOut, file = "data/trainingOutWithDays.csv")

write.csv(newValidationInRNN, file = "data/validationInWithDays.csv")
write.csv(newValidationOut, file = "data/validationOutWithDays.csv")


## Practice
# inputs <- trainingInputsRNN[trainingInputsRNN$PTID_Key == 694,]
# targets <- trainingTargets[trainingTargets$PTID_Key == 694,]
# 
# newFrames = countDays(inputs, targets)
# inputs <- newFrames[1]
# targets <- newFrames[2]
