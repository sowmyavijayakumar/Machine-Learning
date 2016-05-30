library(caret)
library(PerformanceAnalytics)
library(utils)
library(gdata)
library(reshape)
library(ggplot2)
library(reshape2)

setwd("C:/Users/Sowmya/Desktop/NUI GALWAY/Machine Learning/Assignment 1")
illness<-read.csv("illness.txt", header = FALSE)

rownames(illness) <- c("plasma_glucose", "bp", "test_result", "skin_thickness", "num_pregnancies", "insulin", "bmi", "pedigree", "age")
illness<-as.data.frame(t(illness)) #Transposing the data


inTrainingSet <- createDataPartition(illness$test_result, p = 0.05, list = FALSE)

illnessTrain<-illness[inTrainingSet,]
illnessTest <- illness[-inTrainingSet,]
prop.table(table(illnessTrain$test_result))*100
prop.table(table(illnessTest$test_result))*100

trainX<-illnessTrain[,names(illnessTrain)!="test_result"]
illnessTrain[]

trainX$plasma_glucose<-as.numeric(trainX$plasma_glucose)
trainX$bp<-as.numeric(trainX$bp)
trainX$skin_thickness<-as.numeric(trainX$skin_thickness)
trainX$num_pregnancies<-as.numeric(trainX$num_pregnancies)
trainX$insulin<-as.numeric(trainX$insulin)
trainX$bmi<-as.numeric(trainX$bmi)
trainX$pedigree<-as.numeric(trainX$pedigree)
trainX$age<-as.numeric(trainX$age)

preProcValues<-preProcess(x=trainX,method = c("center","scale"))
preProcValues

illnessTrain$plasma_glucose<-as.numeric(illnessTrain$plasma_glucose)
illnessTrain$bp<-as.numeric(illnessTrain$bp)
illnessTrain$skin_thickness<-as.numeric(illnessTrain$skin_thickness)
illnessTrain$num_pregnancies<-as.numeric(illnessTrain$num_pregnancies)
illnessTrain$insulin<-as.numeric(illnessTrain$insulin)
illnessTrain$bmi<-as.numeric(illnessTrain$bmi)
illnessTrain$pedigree<-as.numeric(illnessTrain$pedigree)
illnessTrain$age<-as.numeric(illnessTrain$age)

illnessTest$plasma_glucose<-as.numeric(illnessTest$plasma_glucose)
illnessTest$bp<-as.numeric(illnessTest$bp)
illnessTest$skin_thickness<-as.numeric(illnessTest$skin_thickness)
illnessTest$num_pregnancies<-as.numeric(illnessTest$num_pregnancies)
illnessTest$insulin<-as.numeric(illnessTest$insulin)
illnessTest$bmi<-as.numeric(illnessTest$bmi)
illnessTest$pedigree<-as.numeric(illnessTest$pedigree)
illnessTest$age<-as.numeric(illnessTest$age)



ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(test_result~., data = scale(illnessTrain), method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
#Output of kNN fit
#knnFit
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)
knnPredict <- predict(knnFit,newdata = illnessTest)


#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, illnessTest$test_result )
mean(knnPredict == illnessTest$test_result)


#Random Forest

ctrl <- trainControl(method="repeatedcv",repeats = 3)
rfFit <- train(test_result ~ ., data = illnessTrain, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
rfFit
plot(rfFit)

#Confusion Matrix
rfPredict <- predict(rfFit,newdata = illnessTest )
confusionMatrix(rfPredict, illnessTest$test_result )


#knn
Accuracy<-c( 0.6591,0.6834,0.694,0.7037 ,0.7073,0.7041,0.7099,0.7097,0.7091, 0.7059)
 Trainingvalue<- c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95) 
 Accuracy = Accuracy * 100
 Trainingvalue_percentage = Trainingvalue * 100
 
 qplot(y=Accuracy,x=Trainingvalue_percentage,geom = "line")

#rf
 Accuracy<-c( 0.6583,0.6928,0.7297,0.7408,0.7403,0.7510,0.7557,0.7609,0.7655, 0.7647)
 Trainingvalue<- c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95) 
 Accuracy = Accuracy * 100
 Trainingvalue_percentage = Trainingvalue * 100
 
 qplot(y=Accuracy,x=Trainingvalue_percentage,geom = "line")
 
 