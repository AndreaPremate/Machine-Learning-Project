Preprocessing <- function()
{
  #Check , install pkg
  source("R_Script/Preprocessing.R")
  check_packages(c("readr"))
  library(readr)
  #caricamento dataset
  data <- read.csv("Dataset/HTRU_2.csv", header=TRUE, sep=",")
  DatasetTrainSet <- read.csv("Dataset/DatasetTrainset.csv", header=TRUE, sep=",")
  DatasetTestSet <- read.csv("Dataset/DatasetTestset.csv", header=TRUE, sep=",")
  #creazione boxplot covariate
  plotdata = scale(DatasetTrainSet[,1:8])
  jpeg(filename="Graph/BoxPlotCovariate.jpg",width=1280,height=720)
  boxplot(plotdata, main = "BoxPlol Covariate",las = 2, col = c("orange"), border = "brown", horizontal = TRUE, notch = TRUE )
  dev.off()
  #creazione PCA
  source("R_Script/Preprocessing.R")
  DatasetPca=PcaFun(DatasetTrainSet, DatasetTestSet)
  DatasetPcaTrainSet=DatasetPca$train
  DatasetPcaTestSet=DatasetPca$test
  #convert to write
  DatasetPcaTrainSet=data.frame(DatasetPcaTrainSet)
  DatasetPcaTestSet=data.frame(DatasetPcaTestSet)
  #renama della variabile HTRU2 in pulsar
  colnames(data)[9]= "PULSAR"
  #save dataset , uso la liberia readr per evitar di salvare l'index
  write_csv(data, "Dataset/Dataset.csv")
  #save trainset con pca , uso la liberia readr per evitar di salvare l'index
  write_csv(DatasetPcaTrainSet, "Dataset/Dataset3PCATrainset.csv")
  #save tesetset con pca , uso la liberia readr per evitar di salvare l'index
  write_csv(DatasetPcaTestSet, "Dataset/Dataset3PCATestset.csv")
  
  #Creazione grafico a torta HTRU2
  source("R_Script/CreateGraphic.R")
  GraficoATortaPULSAR(data,"green", "yellow" ,"Graph/GraficoATortaTarget.jpg","Grafico PULSAR")
  GraficoATortaPULSAR(DatasetTrainSet, "red", "orange", "Graph/GraficoATortaTargetTrainSet.jpg", "Grafico PULSAR Trainset")
  GraficoATortaPULSAR(DatasetTestSet, "blue", "pink" , "Graph/GraficoATortaTargetTestSet.jpg", "Grafico PULSAR Testset")
  
}

#funzione pca 
PcaFun  <- function(train, test){
  #salvo collona pulsar
  train.PULSAR = train$PULSAR
  test.PULSAR = test$PULSAR
  
  #Check , install pkg
  source("R_Script/Preprocessing.R")
  check_packages(c("FactoMineR", "factoextra"))
  library("FactoMineR")
  library("factoextra")
  
  #dataset senza target
  train.active <- train[, 1:8]
  test.active <- test[, 1:8]
  #pca per grafici
  res.pca = PCA(train.active, graph = FALSE)
  res.pca$ind$coord = res.pca$ind$coord * (-1)
  res.pca$var$coord[, 2] = res.pca$var$coord[, 2]*(-1)
  #pca sul train e test
  prin_comp <- prcomp(train.active, scale. = T)
  train <- predict(prin_comp, newdata = train.active)
  train <- as.data.frame(train)
  test <- predict(prin_comp, newdata = test.active)
  test <- as.data.frame(test)
  #creazioni grafici PCA
  source("R_Script/CreateGraphic.R")
  GraficiPCA(res.pca)
  #formare il return 
  train=train[,1:3]
  train=cbind(train, PULSAR= train.PULSAR)
  test=test[,1:3]
  test=cbind(test, PULSAR= test.PULSAR)
  listReturn=list(train,test)
  names(listReturn)=c("train", "test")
  return (listReturn)
}


#funzione controllare packages e install
check_packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}