#-----------------------------------------------------------------
# Author: Collaborative work "madRid" team.
# Date: 2015-09-04
# Purpose: Pump-it-up failure prediction.
# Details: http://www.drivendata.org/competitions/7/page/25/
# Pump-Status: functional - functional needs repair - not functional  
# Files: training.csv - trainingStatus.csv - testing.csv - SubmissionFormat.csv 
# Output: One ".RData" file for each model and group (3 groups considered, each with around 14 algorithms).
#-----------------------------------------------------------------
#Caret methods:
#https://cran.rstudio.com/web/packages/caret/news.html
#http://topepo.github.io/caret/modelList.html
#http://topepo.github.io/caret/Bagging.html
#-----------------------------------------------------------------



#----------------------------------------------------
# WORKING DIRECTORY
#----------------------------------------------------
#Working directory
setwd("INCLUIR DIRECTORIO DE TRABAJO")

#----------------------------------------------------
# DATA LOADING
#----------------------------------------------------
#Load training  and trainingStatus data 
library(data.table)
datTrain <- fread("training.csv")
datStat <- fread("trainingStatus.csv")
datIn <- datTrain[ , status_group:=datStat$status_group]
datIn <- as.data.frame(datIn)



#----------------------------------------------------
# DATA MUNGING - CLEANING
#----------------------------------------------------
#Load caret package
library(caret)
set.seed(1234)
colBad <- nearZeroVar(datIn)
datInclean <- datIn[, -colBad[2:length(colBad)]]
fehowold <- ifelse(datInclean$construction_year==0,0, 2015-datInclean$construction_year)
toOut <- c(1,2,3,4,7,8,9,10,11,13,14,15,16,18,19,20,21,23,24,26,28,29,32,34,35,37)

datGod <- datInclean[, -toOut]
datGod$fehowold <- fehowold
datGod$festatus <- datGod$status_group
library(stringr)
datGod$festatus <- str_replace_all(datGod$festatus," ","_")
datGod$festatus <- as.factor(datGod$festatus)
datGod <- datGod[, -c(12)] 
datGod <- datGod[, -c(2)] 

set.seed(1)
sizMod <- 1 * nrow(datGod)
datSamp <- datGod[sample(1:nrow(datGod), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]


#-------------------------------------------------------------------------
#---------------------  ALL MODELS - 2 CLASSES  --------------------------
#-------------------------------------------------------------------------
myTrain <- trainDat[trainDat$festatus!="functional_needs_repair",]
myTrain$festatus <- as.factor(as.vector(myTrain$festatus))
myTest <- testDat[testDat$festatus!="functional_needs_repair",]
myTest$festatus <- as.factor(as.vector(myTest$festatus))

modeLos <- c(
             'rf', 'fda', 'pls', 'gpls', 'xgbTree', 'nnet',
             'glmboost', 'plsRglm', 'rknn', 'extraTrees','gbm',
             'rpart', 'Boruta', 'RRF', 'gamSpline', 'C5.0', 'C5.0Tree'
             )

for(i in 1:length(modeLos)) {
  
  a <- Sys.time(); a
  set.seed(1)
  
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE,  verboseIter = FALSE )
  modFitxxx <- train(x=myTrain[,1:11], y=myTrain[, 12], method=modeLos[i], tuneLength = 10, metric="ROC", trControl = cvCtrl )
  
  predxxx <- predict(modFitxxx, myTest)
  conMatxxx <- confusionMatrix(predxxx, myTest$festatus)
  
  b <- Sys.time(); b; b-a
  
  save(conMatxxx, modFitxxx, file=paste("modFit",modelos[i],"_2_class.RData",sep="" ) )
 
}

  #just for SVM
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE,  verboseIter = FALSE )
  svmGrid = expand.grid( .C = c(1, 5, 10, 50, 100), .sigma = c(0.001, 0.005, 0.01, 0.05))
  modFitsvm <- train(x=myTrain[,1:11], y=myTrain[, 12], method="svmRadial",tuneGrid = svmGrid, tuneLength = 10, metric="ROC", trControl = cvCtrl ) 
  predSvm <- predict(modFitsvm, myTest)
  conMatsvm <- confusionMatrix(predSvm, myTest$festatus)
  save(conMatsvm, modFitsvm, file="modFitsvm_2_class.RData" )

#------------------------------------------------------------------------------
#--------------------- END - ALL MODELS - 2 CLASSES  --------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#---------------------  ALL MODELS - 3 CLASSES - NOT BALANCED -----------------
#------------------------------------------------------------------------------

modeLos <- c(
  'rf', 'xgbTree', 'nnet',
  'glmboost', 'plsRglm', 'rknn', 'extraTrees','gbm',
  'rpart', 'Boruta', 'RRF', 'gamSpline', 'C5.0Tree', 'C5.0'
)

for(i in 1:length(modeLos)) {
  
  a <- Sys.time(); a
  set.seed(1)
  
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, classProbs = TRUE,  verboseIter = FALSE )
  modFitxxx <- train(x=trainDat[,1:11], y=trainDat[, 12], method=modeLos[i], tuneLength = 10, metric="Accuracy", trControl = cvCtrl )
  
  predxxx <- predict(modFitxxx, testDat)
  conMatxxx <- confusionMatrix(predxxx, testDat$festatus)
  
  b <- Sys.time(); b; b-a
  
  save(conMatxxx, modFitxxx, file=paste("modFit",modelos[i],"_3_class_notbal.RData",sep="" ) )
  
}

  #just for SVM
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, classProbs = TRUE,  verboseIter = FALSE )
  svmGrid = expand.grid( .C = c(1, 5, 10, 50, 100), .sigma = c(0.001, 0.005, 0.01, 0.05))
  modFitsvm <- train(x=trainDat[,1:11], y=trainDat[, 12], method="svmRadial",tuneGrid = svmGrid, tuneLength = 10, metric="Accuracy", trControl = cvCtrl ) 
  predSvm <- predict(modFitsvm, testDat)
  conMatsvm <- confusionMatrix(predSvm, testDat$festatus)
  save(conMatsvm, modFitsvm, file="modFitsvm_3_class_notbal.RData" )

#------------------------------------------------------------------------------
#------------------ END - ALL MODELS - 3 CLASSES - NOT BALANCED ---------------
#------------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#---------------------  ALL MODELS - 3 CLASSES -  BALANCED -------------------
#-----------------------------------------------------------------------------
datUp <- upSample(datGod[,1:11], datGod[,12]) 
names(datUp)[12] <- c('festatus') 

set.seed(1)
sizModup <- 1 * nrow(datUp)
datSampUp <- datUp[sample(1:nrow(datUp), sizModup) , ]

inTrainup <- createDataPartition(datSampUp$festatus, p=0.70)[[1]]
trainDatup <- datSampUp[ inTrainup, ]
testDatup <- datSampUp[ -inTrainup, ]


modeLos <- c(
  'rf', 'xgbTree', 'nnet',
  'glmboost', 'plsRglm', 'rknn', 'extraTrees','gbm',
  'rpart', 'Boruta', 'RRF', 'gamSpline', 'C5.0Tree', 'C5.0'
)

for(i in 1:length(modeLos)) {
  
  a <- Sys.time(); a
  set.seed(1)
  
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, classProbs = TRUE,  verboseIter = FALSE )
  modFitxxx <- train(x=trainDatup[,1:11], y=trainDatup[, 12], method=modeLos[i], tuneLength = 10, metric="Accuracy", trControl = cvCtrl )
  
  predxxx <- predict(modFitxxx, testDatup)
  conMatxxx <- confusionMatrix(predxxx, testDatup$festatus)
  
  b <- Sys.time(); b; b-a
  
  save(conMatxxx, modFitxxx, file=paste("modFit",modelos[i],"_3_class.RData",sep="" ) )
  
}

  #just for SVM
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, classProbs = TRUE,  verboseIter = FALSE )
  svmGrid = expand.grid( .C = c(1, 5, 10, 50, 100), .sigma = c(0.001, 0.005, 0.01, 0.05))
  modFitsvm <- train(x=trainDatup[,1:11], y=trainDatup[, 12], method="svmRadial",tuneGrid = svmGrid, tuneLength = 10, metric="Accuracy", trControl = cvCtrl ) 
  predSvm <- predict(modFitsvm, testDatup)
  conMatsvm <- confusionMatrix(predSvm, testDatup$festatus)
  save(conMatsvm, modFitsvm, file="modFitsvm_3_class_bal.RData" )

#------------------------------------------------------------------------------
#------------------ END - ALL MODELS - 3 CLASSES - BALANCED -------------------
#------------------------------------------------------------------------------

#-----------------------------------------------------
#------------------ END OF PROGRAM -------------------
#-----------------------------------------------------
