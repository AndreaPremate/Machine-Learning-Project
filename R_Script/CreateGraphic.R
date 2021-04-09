#Creazione e salvataggio grafico a torta di PULSAR 
GraficoATortaPULSAR <- function(dataset, color1, color2, path, name)
{
  check_packages(c("ggplot2"))
  library(ggplot2)
  table1=table(dataset$PULSAR)
  cols<-c(color1, color2)
  labs<-c("Negativo" , "Positivo")
  pct<-round((table1/margin.table(table1)*100),1)
  lbls<-paste(pct,"%",sep="")
  jpeg(filename=path,width=1280,height=720)
  pie(table1, main=name, labels = lbls, col=cols)
  legend(1.0, 1.0, cex = 0.8, legend=labs, fill = cols)
  dev.off()
  rm(cols,labs, lbls, pct, table1)
}

#Creazione grafici PCA
GraficiPCA <- function(res.pca)
{
  jpeg(filename="Graph/PCA1.jpg")
  print(fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)))
  dev.off()
  
  jpeg(filename="Graph/PCA2.jpg",width=1280,height=720)
  print(fviz_pca_var(res.pca, col.var = "black"))
  dev.off()
  
  jpeg(filename="Graph/PCA3.jpg",width=1280,height=720)
  #plotta tutte le istanze sulle prime 2 componenti principali
  print(fviz_pca_ind(res.pca, col.ind = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               label = "none"
  ))
  dev.off()
  
  jpeg(filename="Graph/PCA4.jpg",width=1280,height=720)
  #plotta un campione di istanze sulle prime 2 componenti principali
  ind_sample = res.pca[["ind"]][["coord"]][sample(nrow(res.pca[["ind"]][["coord"]]), 500), ]
  plot(ind_sample[,1], ind_sample[,2])
  dev.off()
}

#Creazione grafici PCA
GraficoAlbero <- function(decisionTree , x)
{
  if(x==1)
    jpeg(filename="Graph/DecisionTree.jpg",width=1280,height=720)
  else
    jpeg(filename="Graph/DecisionTreePCA.jpg",width=1280,height=720)
  fancyRpartPlot(decisionTree)
  dev.off()
}