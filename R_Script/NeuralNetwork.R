NeuralNetwork3PCA <- function(trainset, testset) {
  #Controllo e install pkg
  check_packages(c("neuralnet", "caret"))
  library(neuralnet)
  library(caret)
  #Definisco il train control
  train_control= trainControl(method="cv", number=10 )
  #definiamo pesi iniziali
  matweight=matrix(nrow=4, ncol=2)
  matweight[,1]=c(-0.5261514 ,-0.2908128, -1.0488022, 1.7975449)
  matweight[,2]=c(0.9118313, 0.3164780, 0.9249827, 1.6249071)
  #Train del modello tramite la 10-fold
  startTime=Sys.time() #orario inizio
  model=train(PULSAR ~ .,
              data=trainset, trControl=train_control, startweights = matweight,
              hidden=2, method="nnet")
  endTim=Sys.time() #orari fine
  timeOccTrain=endTim-startTime # tempo inpiegato
  #Save relazione neuroni nascosti-accuracy
  jpeg(filename="Graph/NeuralNetworkHiddenUnit-Accuracy.jpg",width=1280,height=720)
  print(plot(model))
  dev.off()
  
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
  
  #formare la lista di return
  listReturn=list(model, predictTrain, accuracyTrain , timeOccTrain, predictTest, accuracyTest, timeOccPredictTest, timeOccPredictTrain)
  names(listReturn)=c("model", "predictTrain", "accuracyTrain" , "timeOccTrain", "predictTest", "accuracyTest", "timeOccPredictTest", "timeOccPredictTrain")
  return(listReturn)
}