#Model Evaluation
ModelEvaluation <- function(pred, lab, model_name)
{ 
  check_packages(c("cvms", "caret", "rsvg", "ggimage"))
  library(caret)
  library(cvms)
  library(rsvg)
  library(ggimage)
  #crazione matrice predictiva e matrice di confusione
  matricePredict=table(pred, lab)
  confusionMat= confusionMatrix(matricePredict, positive="1")
  
  #formatazzione matrice e rename
  dataConfMat= data.frame(confusionMat$table)
  colnames(dataConfMat)[1]=("Prediction")
  colnames(dataConfMat)[2]=("Target")
  if(model_name=="Decision Tree"){
    pathname="Graph/MatriceConfusioneDecisionTree.jpg"
  }else if(model_name=="Decision Tree PCA"){
    pathname= "Graph/MatriceConfusioneDecisionTree3PCA.jpg"
  }else if(model_name=="Neural Network PCA"){
    pathname= "Graph/NeuralNetwork3PCA.jpg"
  }else{
    pathname= "Graph/UNDEFINED.jpg"
  }
  #Creazione plot matrice confusione

  jpeg(filename=pathname,width=1280,height=720)
  print(plot_confusion_matrix(dataConfMat, prediction_col = "Prediction", target_col = "Target", counts_col = "Freq", add_normalized = FALSE)
        +ggtitle(paste("Confusion Matrix", model_name)))
  dev.off()
  #precisione
  precision= confusionMat$byClass[5]
  #recall
  recall= confusionMat$byClass[6]
  #f-measure
  f_measure= confusionMat$byClass[7]
  
  listReturn=list(precision, recall, f_measure, confusionMat)
  names(listReturn)=c("Precision", "Recall", "F1", "confusionMatrix")
  return(listReturn)
}


#creazione auc e roc
roc_function <- function(pred, lab, model_name)
{
  check_packages(c("pROC"))
  library(pROC)
  if(model_name=="Decision Tree"){
    pathname="Graph/ROCDecisionTree.jpg"
  }else if(model_name=="Decision Tree PCA"){
    pathname= "Graph/ROCDecisionTree3PCA.jpg"
  }else if(model_name=="Neural Network PCA"){
    pathname= "Graph/ROCNeuralNetwork3PCA.jpg"
  }else{
    pathname= "Graph/ROCUNDEFINED.jpg"
  }
  #label target
  pulsar=factor(lab$PULSAR)
  pulsar_true= as.numeric(pulsar==1)
  #predict
  pred=factor(pred)
  pulsar_pred= as.numeric(pred==1)
  #roc e save del plot
  labdata = data.frame(pulsar_true, pulsar_pred)
  jpeg(filename=pathname,width=720,height=720)
  print(res <- roc(pulsar_true ~ pulsar_pred,labdata, plot=TRUE, levels=c("0", "1"),direction = "<"))
  dev.off()
  #auc
  auc = res$auc
  return(auc)
}
