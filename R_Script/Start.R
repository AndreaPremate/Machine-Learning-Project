# Set working directory
current_path <-dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_path)
rm(current_path)

#Proprocessing iniziale del dataset
source("R_Script/Preprocessing.R")
Preprocessing()
#Rimozione funzione Processing
rm(Preprocessing)

#caricamento dataset
trainset3PCA=read.csv("Dataset/Dataset3PCATrainset.csv", header=TRUE)#trainset con la pca
trainset3PCA$PULSAR=factor(trainset3PCA$PULSAR)
testset3PCA=read.csv("Dataset/Dataset3PCATestset.csv", header=TRUE)#testset con la pca
testset3PCA$PULSAR=factor(testset3PCA$PULSAR)
#caricamento trainset e testset
trainset=read.csv("Dataset/DatasetTrainset.csv", header=TRUE)#trainset 
trainset$PULSAR=factor(trainset$PULSAR)
testset=read.csv("Dataset/DatasetTestset.csv", header=TRUE)#testset
testset$PULSAR=factor(testset$PULSAR)

#Alberi
source("R_Script/DecisionTree.R")
decisionTree=DecisionTree(trainset, testset , "DecisionTree")
decisionTree3PCA=DecisionTree(trainset3PCA,testset3PCA , "DecisionTree3PCA")
source("R_Script/ModelEvaluation.R")
#Valutazione alberi
decisionTreeModEvalut = ModelEvaluation(decisionTree$predictTrain, trainset$PULSAR, "Decision Tree")
decisionTreeAuc=roc_function(decisionTree$predictTrain, trainset, "Decision Tree")
decisionTree3PCAModEvalut = ModelEvaluation(decisionTree3PCA$predictTrain, trainset3PCA$PULSAR, "Decision Tree PCA")
decisionTree3PCAAuc=roc_function(decisionTree3PCA$predictTrain, trainset3PCA, "Decision Tree PCA")

#NeuralNetwork 
source("R_Script/NeuralNetwork.R")
neuralNetwork=NeuralNetwork3PCA(trainset3PCA, testset3PCA)
source("R_Script/ModelEvaluation.R")
#Valutazione rete neurale
neuralNetwork3PCAModEvalut = ModelEvaluation(neuralNetwork$predictTrain, trainset3PCA$PULSAR, "Neural Network PCA")
neuralNetwork3PCAAuc=roc_function(neuralNetwork$predictTrain, trainset3PCA, "Neural Network PCA")

#t-test per determinare se le medie sono uguali
x=c(decisionTree$accuracyTrain, decisionTree$accuracyTest, decisionTreeModEvalut$Precision, decisionTreeModEvalut$Recall, decisionTreeModEvalut$F1)
y=c(decisionTree3PCA$accuracyTrain, decisionTree3PCA$accuracyTest, decisionTree3PCAModEvalut$Precision, decisionTree3PCAModEvalut$Recall, decisionTree3PCAModEvalut$F1)
z=c(neuralNetwork$accuracyTrain, neuralNetwork$accuracyTest, neuralNetwork3PCAModEvalut$Precision, neuralNetwork3PCAModEvalut$Recall, neuralNetwork3PCAModEvalut$F1)
#t-test decision tree con decision tree PCA
t_test_DecTree_DecTreePCA=t.test(x, y, alternative = "less", paired= TRUE)
t_test_DecTree_DecTreePCA$p.value
#t-test decision tree con rete  neurale PCA
t_test_DecTree_NN3PCA=t.test(x, z, alternative = "less", paired= TRUE)
t_test_DecTree_NN3PCA$p.value
#t-test decision tree PCA con rete  neurale PCA
t_test_DecTreePCA_NN3PCA=t.test(z, y, alternative = "less", paired= TRUE)
t_test_DecTreePCA_NN3PCA$p.value


#salvo i tempi di esecuzione e tempi predic in formato xlsx
check_packages(c("xlsx", "data.table"))
library(xlsx)
library(data.table)
timeState=Sys.time()
timeOccTrain= c(decisionTree$timeOccTrain, decisionTree3PCA$timeOccTrain, neuralNetwork$timeOccTrain)
timeOccPredTest= c(decisionTree$timeOccPredictTest, decisionTree3PCA$timeOccPredictTest, neuralNetwork$timeOccPredictTest)
timeOccPredTrain= c(decisionTree$timeOccPredictTrain, decisionTree3PCA$timeOccPredictTrain, neuralNetwork$timeOccPredictTrain)
timetab=data.frame(timeOccTrain, timeOccPredTest, timeOccPredTrain)
colnames(timetab)= c("Train (s)", "Predict test (s)", "Predict train (s)")
rownames(timetab)= c("Decision Tree", "Decision Tree PCA", "Neural Network PCA")
TitleOfTimeData=gsub(":", ";",((as.character(paste("Time_Spent_Model/", as.character(timeState),".xlsx", sep = "")))))
write.xlsx(timetab, TitleOfTimeData,col.names = TRUE, row.names = TRUE, append = FALSE)

#Salvo i risultati e stime dei modelli in formato xlsx
v1=c(decisionTree$accuracyTest, decisionTree$accuracyTrain, decisionTreeModEvalut$Precision ,decisionTreeModEvalut$Recall , decisionTreeModEvalut$F1 ,decisionTreeAuc)
v2=c(decisionTree3PCA$accuracyTest, decisionTree3PCA$accuracyTrain, decisionTree3PCAModEvalut$Precision, decisionTree3PCAModEvalut$Recall, decisionTree3PCAModEvalut$F1, decisionTree3PCAAuc)                  
v3=c(neuralNetwork$accuracyTest, neuralNetwork$accuracyTrain, neuralNetwork3PCAModEvalut$Precision, neuralNetwork3PCAModEvalut$Recall, neuralNetwork3PCAModEvalut$F1, neuralNetwork3PCAAuc)
resultMes=data.frame(v1,v2,v3)
colnames(resultMes)= c("Decision Tree", "Decision Tree PCA", "Neural Network PCA")
rownames(resultMes)= c("Accuracy (test) ", "Accuracy (10-fold cv)", "Precision", "Recall", "F-measure", "AUC")
TitleOfResultData=gsub(":", ";",((as.character(paste("Result/", as.character(timeState),".xlsx", sep = "")))))
write.xlsx(resultMes, TitleOfResultData,col.names = TRUE, row.names = TRUE, append = FALSE)

#Salvo imagine workspace
save.image(file = "Image_Workspace/ImgWorkspace.RData")
#comando per caricare Image_workspace
#load(file = "Image_Workspace/ImgWorkspace.RData")
