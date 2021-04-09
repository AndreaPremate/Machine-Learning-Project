#Albero con il dataset originale
DecisionTree <- function(trainset, testset ,name)
{ 
  check_packages(c("rpart", "rattle", "rpart.plot", "RColorBrewer", "caret"))
  library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  library(caret)
  #Definisco il train control
  train_control= trainControl(method="cv", number=10)
  #Train del modello tramite la 10-fold
  startTime=Sys.time() #orario inizio
  model=train(PULSAR ~ .,
              data=trainset, trControl=train_control, method="rpart")
  endTim=Sys.time() #orari fine
  timeOccTrain=endTim-startTime # tempo inpiegato
  #predict sul testset
  startTime=Sys.time() #orario inizio
  predictTest=predict(model$finalModel, testset, type = "class")
  endTim=Sys.time() #orari fine
  timeOccPredictTest=endTim-startTime # tempo inpiegato
  #predict sul trainset
  startTime=Sys.time() #orario inizio
  predictTrain=predict(model$finalModel, trainset, type = "class")
  endTim=Sys.time() #orari fine
  timeOccPredictTrain=endTim-startTime # tempo inpiegato
  
  #accuratezza e media
  accuracyCV=model$resample['Accuracy']$Accuracy
  accuracyTrain=mean(accuracyCV)
  #matrice di confusione e accuratezza
  confusion.matrix = confusionMatrix(table(predictTest, testset$PULSAR))
  accuracyTest=confusion.matrix$overall['Accuracy']
  if(name== 'DecisionTree3PCA'){
       source("R_Script/CreateGraphic.R")
       GraficoAlbero(model$finalModel, 2)
    }
  else{
      source("R_Script/CreateGraphic.R")
      GraficoAlbero(model$finalModel, 1)
  }
  #formare la lista di return
  listReturn=list(model, predictTrain, accuracyTrain , timeOccTrain, predictTest, accuracyTest, timeOccPredictTest,  timeOccPredictTrain)
  names(listReturn)=c("model", "predictTrain", "accuracyTrain" , "timeOccTrain", "predictTest", "accuracyTest", "timeOccPredictTest", "timeOccPredictTrain")
  return(listReturn)
}
