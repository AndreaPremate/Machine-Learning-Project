Link dataset: https://archive.ics.uci.edu/ml/datasets/HTRU2


!!! Se si utilizza un sistema a 64-bit è neccesario avere java-64bit installato sulla macchina per la scrittura dei
file contenenti i risultati in formato .xlsx !!!

Per avviare il progetto avviare il filre Start.R situato nella cartella R_Script



Struttura del progetto:

"Dataset" cartella contentente i vari dataset
	HTRU_2 dataset iniziale
	DatasetTrainset dataset creato prima della lavorazione in modo proporzionato per il trainset (70%)
	DatasetTestset dataset creato prima della lavorazione in modo proporzionato per il testset (30%)
	Dataset3PCATrainset dataset creato nel preprocessing tramite il DatasetTrainset 
	Dataset3PCATestset dataset creato nel processing tramite DatasetTestset usando la PCA usata per il Dataset3PCATrainset 
	Dataset dataset creato per la lavorazione statistica

"Graph" cartella contente i grafici creati in automatico

"R_Script" cartella contente i script
	CreateGraphic- script per creare i vari grafici
	DecisionTree- script per creare i modelli dei alberi
	ModelEvaluation- script per model evalutation
	NeuralNetwork- script per creare la rete neurale
	Preprocessing- script di preprocessing
	Start- script di avvio

"Result" cartella contente i risultati dei modelli(salvate in base al orario di fine esecuzione) 
	
"Time_Spent_Model" cartella contente i risultati temporali dei modelli(salvate in base al orario di fine esecuzione) 
