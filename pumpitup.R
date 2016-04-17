#-----------------------------------------------------------------
# Author: Carlos Ortega
# Date: 2015-08-13
# Purpose: Pump-it-up failure prediction.
# Details: http://www.drivendata.org/competitions/7/page/25/
# Pump-Status: functional - functional needs repair - not functional  
# Files: training.csv - trainingStatus.csv - testing.csv - SubmissionFormat.csv 
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
setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp")
#setwd("/Volumes/STORE N GO/R-cosas/2015-08 - PumpItUp")
#load("xgb_glm_knn_nnet_RF_gbm_C50_lasso_fda_plr_Rpart_train_test.RData")
#load("xgb_gbm_rf_plr_mda_pls_025_3status_Upscale.RData")


#----------------------------------------------------
# DATA LOADING
#----------------------------------------------------
#Load training  and trainingStatus data 
library(data.table)
datTrain <- fread("training.csv")
datStat <- fread("trainingStatus.csv")
datIn <- datTrain[ , status_group:=datStat$status_group]
datIn <- as.data.frame(datIn)
#all.equal(datTrain$id, datStat$id)

datTesting <- fread("testing.csv")
datTesting <- as.data.frame(datTesting)

# #Which are the levels/cases that are present in columns of datTesting and not in datIn
#  dftmp <- data.frame()
#  cont <- 0
#  for( i in 1:ncol(datTesting)) {
#    cltmp <- class(datIn[, i])
#    if(cltmp == "character" & names(datIn)[i] != "date_recorded") {
#        cont <- cont+1
#        uniqdatIn <- unique(datIn[,i]) 
#        uniqdatTs <- unique(datTesting[,i])
#        dftmp[cont,1] <- names(datIn)[i]
#        dftmp[cont,2] <- length(uniqdatIn)
#        dftmp[cont,3] <- length(uniqdatTs)
#        dftmp[cont,4] <- sum(!(uniqdatTs %in% uniqdatIn))
#    } else next
#  }
# #Differences are here
# dftmp[dftmp$V4>0,]
# V1    V2    V3   V4
# 1       funder  1898   981  243
# 2    installer  2146  1092  265
# 3     wpt_name 37400 10840 8284
# 5   subvillage 19288  8444 2138
# 8         ward  2092  1959    6
# 11 scheme_name  2697  1790  172

#----------------------------------------------------
# DATA MUNGING - CLEANING
#----------------------------------------------------
#Let's model
#Load caret package
library(caret)
set.seed(1234)
#Clean meaningless columns
colBad <- nearZeroVar(datIn)
#first element of colBad (gps_height in principle is interesting)
datInclean <- datIn[, -colBad[2:length(colBad)]]

#-----------------------------------------------------------
# Added: 2015-08-20 - 05:
# See note "2015-08-20 - 04". New strategy to simplify the dataset.
# To remove:
# id: 1
# amount_tsh: 2 (most of them are 0s)
# date_recorded: 3
# funder: 4 (another more important: installer)
#-----
# problem with installer: 2146 suppliers. Many with just one pump installed.
# It happens that in the test process, it appears suppliers not modeled in the train process.
# Let's model initially without installer
# installer: 6
# Another problem is that too many factors to separate.
#-----
# longitude: 7 / latitude: 8
# wpt_name: 9 / basin: 10  / subvillage: 11 (prefearable to use region)
# As subvillage has 19288 different entries (randomForest, cannot classify more than 53 levels)
# region_code: 13 / district_code: 14 (to use region initially)
# lga: 15 / ward: 16
# public_meeting: 18 (meaning??)
# scheme_management: 19 (is equivalent to management)
# permit: 20 (problems with many NAs difficult to imput them. Not initially)
#-----
# (Analyze failures by extration_type_class)
# (extraction_type_class: 7 classes)
# (extraction_type_group: 13 classes)
# (extraction_type: 18 classes)
# apply(table(datInclean$extraction_type_class, datInclean$status_group), 1, prop.table)*100
# motorpump: 60% bad - other: 83% - wind-power: 56%
# apply(table(datInclean$extraction_type_group, datInclean$status_group), 1, prop.table)*100
# india mark iii: 55% - mono: 61% - other: 83% - other handpump: 52% - other motorpump: 56% - wind-powered: 56%
# apply(table(datInclean$extraction_type, datInclean$status_group), 1, prop.table)*100
# climax: 75% - india mark iii: 54% - ksb: 49% - mono: 61%  - other: 80% - other mkulima..: 100% - other-play-pump: 65% - 
#-----
# extraction_type_group: 23 - extraction_type_class: 24 (extraction_type is more general)
# management_group: 26 (management is more general)
# payment_type: 28 (payment is equivalent)
# water_quality: 29 (quality_group is equivalent)
# quantity_group: 32 (quantity is equivalent)
# source_type: 34 - source_class: 35 (source is more general)
# waterpoint_type_group: 37 (waterpoint_type_group is more general)
# ----------
# To add: howold by using construction_year but there are many values with "0".
# The ones added: feature engineering "fe"...
# fehowold 
fehowold <- ifelse(datInclean$construction_year==0,0, 2015-datInclean$construction_year)
# remove construction_year: 21
# --------
# Status by how old (from 10 to 50 years)
fecuthowold <- cut(fehowold, seq(10,50,10))
# apply(table(fecuthowold, datInclean$status_group), 1, prop.table)*100
# Older than 20 years half are non-functional. Interesting variable.
# To-do: Think about filling rows with values = 0
# ---------
#Status will be "good": functional and "bad": non functional / functional needs repair.
#In that way, status will be more balanced.
#remove also status_group
#toOut <- c(1,2,3,4,7,8,9,10,11,13,14,15,16,18,19,20,21,23,24,26,28,29,32,34,35,37,38)
toOut <- c(1,2,3,4,7,8,9,10,11,13,14,15,16,18,19,20,21,23,24,26,28,29,32,34,35,37)
#toOutxgb <- c(1,2,3,7,8, 18,20)
festatus <- ifelse( (datInclean$status_group=="non functional" | datInclean$status_group=="functional needs repair"), "bad", "good")

#---- Data.frame reduced and enhanced...
datGod <- datInclean[, -toOut]
#datGod <- datInclean[, -toOutxgb]
datGod$fehowold <- fehowold
#datGod$festatus <- as.factor(festatus)

#Leave "status_group" in the original way, but in "festatus" variable.
datGod$festatus <- datGod$status_group
#Remove white spaces in festatus...
library(stringr)
datGod$festatus <- str_replace_all(datGod$festatus," ","_")
datGod$festatus <- as.factor(datGod$festatus)
#Remove "status_group", now status is in "festatus".
datGod <- datGod[, -c(12)] #With toOut
#datGod <- datGod[, -c(31)] #With toOutxgb

#remove "installer"
datGod <- datGod[, -c(2)] #with toOut

#How many classes are in total in datGod??
# dftmpGod <- data.frame()
# cont <- 0
# for( i in 1:ncol(datGod)) {
#   cltmp <- class(datGod[, i])
#   if(cltmp == "character") {
#     cont <- cont+1
#     uniqdatIn <- unique(datGod[,i]) 
#     dftmpGod[cont,1] <- names(datGod)[i]
#     dftmpGod[cont,2] <- length(uniqdatIn)
#   } else next
# }
# sum(dftmpGod$V2)
# [1] 86

# #---- 2015-08-26 to include installer but in a treated way...(column 6)
# # "installer" presents problems with the "testing.csv" file. Do NOT use.
# #some installers repeated capital o lower letters...
# #everything in lower letters.
# insDfori <-  str_to_lower(datGod$installer)
# insDf <- as.data.frame(sort(table(insDfori), decreasing=TRUE ))
# insDf$Inst <- row.names(insDf)
# names(insDf)[1] <- c('Freq')
# row.names(insDf) <- NULL
# #second installer "empty"..?.
# #tenth installer "0"... ?.
# dim(insDf)
# # [1] 1936    2
# # 1936 different installers
# #The 50 first installers accounts for the majority of installations...
# #plot(insDf$Freq, xlim=c(0,100), ylim=c(0,1000))
# #there are some other inconsistences "world", "world bank"...
# #"goverment", "goverment central"...
# namGod <- insDf$Inst[1:50]
# insProc <- ifelse( (insDfori %in% namGod)==TRUE, insDfori, "other")
# #now to remove the "" and "0"
# insRef <- ifelse( (insProc=="" | insProc=="0"), "unknown", insProc)
# #datGod$feinstaller <- insRef #attach to the data.frame - toOut
# datGod$feinstaller <- insDfori #just the names tolower.
# #remove "installer" and "construction_year" and sort the columns
# datGod <- datGod[, c(1, 3:12,14,13)] #with toOut
# #datGod <- datGod[, c(1,2, 4:12,14:31,33,32)] #with toOutxgb

#filter one case in "extraction_type" : "other - mkulima/shinyanga"
#just two records and both are "bad"...
#the existence of this creates problems in PLS y PLR models.
#datGod <- datGod[ datGod$extraction_type!="other - mkulima/shinyanga",]

#datGod$fecuthowold <- fecuthowold (not included initially)
#-----------------------------------------------------------------
# datGod with 30 predictors - Just removed the non-meaningul ones.
dim(datGod)
# [1] 59400    12
head(datGod)
save(
      datGod,
      file="datGod.RData"
)
#-----------------------------------------------------------------

# Just to see if it is possible to model, a 25%  sample of the whole set.
set.seed(1)
#sizMod <- 0.50 * nrow(datGod)
sizMod <- 1 * nrow(datGod)
datSamp <- datGod[sample(1:nrow(datGod), sizMod) , ]


#Although working with the training file, I divide between
#training and testing. Later I will check with the testing file.
inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]


#-------------------------
# EQUALLY BALANCED
#-------------------------
#datUp <- downSample(datGod[,1:11], datGod[,12]) #for toOut - no installer
datUp <- upSample(datGod[,1:11], datGod[,12]) #for toOut - no installer
#datUp <- upSample(datGod[,1:12], datGod[,13]) #for toOut
#datUp <- upSample(datGod[,1:30], datGod[,31]) #for toOutxgb
names(datUp)[12] <- c('festatus') #for toOut
#names(datUp)[13] <- c('festatus') #for toOut
#names(datUp)[31] <- c('festatus') #for toOutxgb
table(datUp$festatus)
# functional functional_needs_repair          non_functional 
# 32259                   32259                   32259 
save(
  datUp,
  file="datUp.RData"
)


# Just to see if it is possible to model, a 25%  sample of the whole set.
set.seed(1)
#sizModup <- 0.25 * nrow(datUp)
sizModup <- 1 * nrow(datUp)
datSampUp <- datUp[sample(1:nrow(datUp), sizModup) , ]

inTrainup <- createDataPartition(datSampUp$festatus, p=0.70)[[1]]
trainDatup <- datSampUp[ inTrainup, ]
testDatup <- datSampUp[ -inTrainup, ]


#to run parallel computations code does not change just need these three lines
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

#clean data
# rm(datIn, datStat, datInclean, datTrain, datGod, datUp, datSamp, datSampUp, insDf)
# rm(festatus, numCor, colBad, fecuthowold, fehowold, insDfori, insProc, insRef, inTrain, inTrainup, namGod, sizMod, sizModup, toOutxgb)
# gc()

#----------------------------------------------------
# DATA MODEL
#----------------------------------------------------


#-----------------------------------------------------------
#With RF following example Caret-userR2014
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl( 
  method = "repeatedcv", repeats = 5,
  #summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE
)

#Model
modFitRF <- train( 
  #festatus ~. , data=trainDat, 
  festatus ~. , data=trainDatup, 
  method = "rf",
  #metric = "ROC",
  metric = "Accuracy",
  tuneLength = 10,
  trControl=cvCtrl,
  #Equivalent to Weka. RF parameters.
  ntree=100, do.trace=TRUE
)

#Prediction
predrf <- predict(modFitRF, newdata=testDat)

#ConfusionMatrix
conMatrf <- confusionMatrix(testDat$festatus, predrf)
#Result: 78% Accuaracy (Specificity: 77% - Sensitivity: 80%)
#Execution time: ~5 hours.
#modFitRF object in workspace occupies 40Mb
#Result: 25% - 3class - Balanced Equally
#Result: 75% Accuracy.
#Execution time: ~ 5 hours.

#Variable Importance
Imprf <- varImp( modFitRF, scale=F)
plot(Imprf, top=20)
b <- Sys.time();b; b-a




#-----------------------------------------------------------
#With dnn - DEEP NEURAL - 2015-08-31
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl( 
                       method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = TRUE
                       )

#Model
modFitdnn <- train( 
                   festatus ~. , data=trainDatup, 
                   method = "dnn",
                   metric = "Accuracy",
                   tuneLength = 10,
                   trControl=cvCtrl
                  )

#Prediction
preddnn <- predict(modFitdnn, newdata=testDatup)

#ConfusionMatrix
conMatdnn <- confusionMatrix(testDatup$festatus, preddnn)
#Result: 25% - 3class - Balanced Equally - all predictors
#Result: % Accuracy.
#Execution time: ~  hours.

#Variable Importance
Impdnn <- varImp( modFitdnn, scale=F)
plot(Impdnn, top=20)
b <- Sys.time();b; b-a
 
#-----------------------------------------------------------
#With gamSplines - Splines - 2015-09-01
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl( 
  method = "repeatedcv", repeats = 5,
  #summaryFunctio = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

#Model
modFitgam <- train( 
  #festatus ~. , data=trainDatup, 
  x = trainDat[,1:11],
  y = trainDat[,12],
  method = "gam",
  metric = "Accuracy",
  tuneLength = 10,
  trControl=cvCtrl
)

#Prediction
predgam <- predict(modFitgam, newdata=testDat)

#ConfusionMatrix
conMatgam <- confusionMatrix(testDat$festatus, predgam)
#Result: 11 variables - allset - 3 classes.
#Result: Error!. "There were missing values in resampled performance measures...
#Result: Same error, "gam", "gamSpline".

#Variable Importance
Impgam <- varImp( modFitgam, scale=F)
plot(Impgam, top=20)
b <- Sys.time();b; b-a


#-----------------------------------------------------------
#With nb - NaiveBayes - 2015-09-01
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl( 
  method = "repeatedcv", repeats = 5,
  #summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

#Model
modFitnb <- train( 
  festatus ~. , data=trainDatup, 
  method = "nb",
  metric = "Accuracy",
  tuneLength = 10,
  trControl=cvCtrl
)

#Prediction
prednb <- predict(modFitnb, newdata=testDatup)

#ConfusionMatrix
conMatnb <- confusionMatrix(testDatup$festatus, prednb)
#Result: 25% - 3class - Balanced Equally - all predictors
#Result: % Accuracy.
#Execution time: ~  hours.

#Variable Importance
Impnb <- varImp( modFitnb, scale=F)
plot(Impnb, top=20)
b <- Sys.time();b; b-a

#-----------------------------------------------------------
#With GBM following example Caret-userR2014
#gbmGrid de JSS paper.
#gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
#                        .n.trees = (1:10)*25, .shrinkage = .1)
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)
#Model
modFitgbm <- train( 
                    #festatus ~. , data=trainDat,
                    #festatus ~. , data=trainDatup,
                    x = trainDat[, 1:11],
                    y = trainDat[, 12], 
                    method="gbm",
                    #metric = "ROC",
                    metric = "Accuracy",
                    tuneLength = 5,
                    trControl=cvCtrl
                   )

#Prediction
predgbm <- predict(modFitgbm, newdata=testDat)
#ConfusionMatrix
conMatgbm <- confusionMatrix(testDat$festatus, predgbm)
#Result: 78% Accuaracy (Specificity: 78% - Sensitivity: 78%)
#Result: 3 classes - Equally balanced - 25% smaples.
#Result: 72% Accuracy.
#Execution time: 12.8 hours!!.
#----
#Result: 11 variables - 3 classes - 25% - not balanced
#Result: % Accuracy
#Execution time:  min.


#Variable Importance
Impgbm <- varImp( modFitgbm, scale=F)
plot(Impgbm, top=20)
b <- Sys.time();b; b-a

save(
  trainDat, testDat, modFitgbm,
  file="gbm_3class_025_nobalan.RData"
)


#-----------------------------------------------------------
# Added: 2015-08-20 - 01
#With RPART following example Caret-userR2014
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)
#Model
modFitRpart <- train(festatus ~ ., data = trainDat,
                                       method = "rpart",
                                       metric = "ROC",
                                       tuneLength = 10,
                                       trControl = cvCtrl)

# plot(modFitRpart, scales = list(x = list(log = 10)))
#Prediction
predrpart <- predict(modFitRpart, testDat)
#ConfusionMatrix
conMatrpart <- confusionMatrix(testDat$festatus, predrpart)
#Result: 75% Accuaracy (Specificity: 78% - Sensitivity: 72%)

#Variable Importance
Imprpart <- varImp( modFitRpart, scale=F)
plot(Imprpart, top=20)


#-----------------------------------------------------------
# Added: 2015-08-20 -02
#With C50 following example Caret-userR2014
set.seed(1)
a <- Sys.time();a
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)

#C5.0 requires tuning parameters for the boosting.
grid <- expand.grid(model = "tree",
                    trials = 1:100,
                    winnow = FALSE)
#Model
modFitC50 <- train(
                     #festatus ~ ., data = trainDat,
                     x = trainDat[, 1:11],
                     y = trainDat[, 12],
                     method = "C5.0",
                     #metric = "ROC",
                     metric = "Accuracy",
                     tuneGrid = grid,
                     trControl = cvCtrl
                     )
#Prediction
predC50 <- predict(modFitC50, newdata=testDat)
#ConfusionMatrix
conMatC50 <- confusionMatrix(predC50, testDat$festatus)
#Result: 77% Accuaracy (Specificity: 68% - Sensitivity: 85%)
#----
#Result: 11 variables - 3 classes - allset
#Result: 78% Accuracy
#Execution time: ~ 30 min.
#----
#Result: 11 variables - 3 classes - allset - downSample
#Result: 68% Accuracy
#Execution time: 9 min.
#----
#Result: 11 variables - 3 classes - allset - upSample
#Result: upSample generates a dataset that is 8 times bigger than downSample.
#Result: % Accuracy
#Execution time:  min.
#----
#Result: 11 variables - 3 classes - 25% - not balanced
#Result: 76% Accuracy
#Execution time: 18 min.

#Variable Importance
ImpC50 <- varImp( modFitC50, scale=F)
plot(ImpC50, top=20)
b <- Sys.time();b; b-a

save(
      trainDat, testDat, modFitC50,
      file="c50_3class_025_nobalan.RData"
)


#-----------------------------------------------------------
# Added: 2015-08-20 - 03
#With SVM following example Jornadas VI
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)
svmGrid = expand.grid( 
  .C = c(1, 5, 10, 50, 100),
  .sigma = c(0.001, 0.005, 0.01, 0.05)
)

#Model
modFitsvm <- train(festatus ~ ., data = trainDat,
                   method = "svmRadial",
                   tuneLength = 9,
                   tuneGrid = svmGrid,
                   metric = "ROC",
                   trControl = cvCtrl)
#Prediction
#predSvm <- predict(modFitsvm, testDat[, names(testDat) != "festatus"])
predSvm <- predict(modFitsvm, testDat)
#ConfusionMatrix
conMatsvm <- confusionMatrix(predSvm, testDat$festatus)
#Result: 77% Accuaracy (Specificity: 86% - Sensitivity: 66%)
#Execution time: ~ 9 hours.... 



#-----------------------------------------------------------
# Added: 2015-08-20 - 06
#With FDA following example Caret-userR2014
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

modFitfda <- train(festatus ~ ., data = trainDat,
                 method = "fda",
                 tuneLength = 12,
                 metric = "ROC",
                 trControl = cvCtrl)
#Prediction
predfda <- predict(modFitfda, testDat)
#ConfusionMatrix
conMatfda <- confusionMatrix(predfda, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 86% - Sensitivity: 60%)

#Variable Importance
Impfda <- varImp( modFitfda, scale=F)
plot(Impfda, top=20)
 
#-----------------------------------------------------------
# Added: 2015-08-30
#With mda 
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

modFitmda <- train(
                   #festatus ~ ., data = trainDat,
                   festatus ~ ., data = trainDatup,
                   method = "mda",
                   tuneLength = 12,
                   #metric = "ROC",
                   metric = "Accuracy",
                   tuneGrid = expand.grid(.subclasses=1:8),
                   trControl = cvCtrl
                   )
#Prediction
predmda <- predict(modFitmda, testDat)
#ConfusionMatrix
conMatmda <- confusionMatrix(predmda, testDat$festatus)
#Result: 3 status - 25% sample
#Result: 73% Accuracy.
#Execution time: 12min. 
#Result: 3 status - 25% sample - Equally balanced
#Result: 60% Accuracy.
#Execution time: 21 min. 

#Variable Importance
Impmda <- varImp( modFitmda, scale=F)
plot(Impmda, top=20)
b <- Sys.time();b; b-a


#-----------------------------------------------------------
# Added: 2015-08-20 - 06
#With PLR (logistic) following example Caret-userR2014
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)

modFitplr <- train(
                   #festatus ~ ., data = trainDat,
                   festatus ~ ., data = trainDatup,
                   method = "multinom",
                   tuneGrid = data.frame(decay = c(0.1, 1, 10, 20, 40)),
                   trace = FALSE, maxit = 1000,
                   #metric = "ROC",
                   metric = "Accuracy",
                   trControl = cvCtrl)
#Prediction
predplr <- predict(modFitplr, testDat)
#ConfusionMatrix
conMatplr <- confusionMatrix(predplr, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 86% - Sensitivity: 64%)
#Result (with installer): 74% Accuaracy (Specificity: 85% - Sensitivity: 61%)...Worse..??
#Execution time: ~ 4 min. (25% sampled - w-installer)
#Execution time: ~ 23 min. (100% sampled - w-installer)
#Result: 3 status - 25% muestra
#Result: 73% Accuracy.
#Execution time: 10min
#Result: 3 status - 25% muestra - Equally balanced.
#Result: 61% Accuracy.
#Execution time: 17 min


#Variable Importance
Impplr <- varImp( modFitplr, scale=F)
plot(Impplr, top=20)
b <- Sys.time();b; b-a

#-----------------------------------------------------------
# Added: 2015-08-21 - 01
#With LASSO (linear) following example Caret-JornadasVI
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

glmnetGrid = expand.grid( 
                         .alpha = 1,
                         .lambda = seq(0.001, 0.1, length=20)
                        )

#Model
modFitlasso <- train(festatus ~ ., data = trainDat,
                   method = "glmnet",
                   tuneGrid = glmnetGrid,
                   metric = "ROC",
                   trControl = cvCtrl)
#Prediction
predlasso <- predict(modFitlasso, testDat)
#ConfusionMatrix
conMatlasso <- confusionMatrix(predlasso, testDat$festatus)
#Result: 76% Accuaracy (Specificity: 86% - Sensitivity: 63%)

#Variable Importance
Implasso <- varImp( modFitlasso, scale=F)
plot(Implasso, top=20)

#-----------------------------------------------------------
# Added: 2015-08-21 - 02
#With NNET following example help Caret::train
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitnnet <- train(festatus ~ ., data = trainDat,
                     method = "nnet",
                     tuneLength = 10,
                     maxit = 100,
                     metric = "ROC",
                     trControl = cvCtrl)
#Prediction
prednnet <- predict(modFitnnet, testDat)
#ConfusionMatrix
conMatnnet <- confusionMatrix(prednnet, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 85% - Sensitivity: 62%)

#Variable Importance
Impnnet <- varImp( modFitnnet, scale=F)
plot(Impnnet, top=20)
#Execution Time: two hours!!.


#-----------------------------------------------------------
# Added: 2015-08-21 - 03
#With PLS following example Caret presentation 2011
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitpls_max <- train(
                    #festatus ~ ., data = trainDatup,
                    x = trainDat[, c(1:11)],
                    y = trainDat[, c(12)],
                    method = "pls",
                    tuneLength = 20,
                    #preProc = c("center", "scale", "BoxCox"),
                    #metric = "ROC",
                    metric = "Accuracy",
                    maximize = TRUE,
                    trControl = cvCtrl
                    )
#Prediction
#predpls <- predict(modFitpls, testDat)
predpls_max <- predict(modFitpls_max, testDat)
#ConfusionMatrix
#conMatpls <- confusionMatrix(predpls, testDat$festatus)
conMatpls_max <- confusionMatrix(predpls_max, testDat$festatus)
#Result: 76% Accuaracy (Specificity: 87% - Sensitivity: 62%)
#Result (with installer): 74% Accuaracy (Specificity: 87% - Sensitivity: 60%)...Worse..??
#Execution Time: 1.59 mins!!.
#Execution Time (whole dataset):
#Result (with installer): 74% Accuaracy (Specificity: 87% - Sensitivity: 60%)...Worse..??
#Result (with installer + maximize=TRUE): 75% Accuaracy (Specificity: 88% - Sensitivity: 60%)...Worse..??
#Result 3 status - 25% sampled.
#Result: 72%
#Execution time: 1.6 min
#Result 3 status - 25% sampled - Equally Balanced.
#Result: 60% ... Baja!!.
#Execution time: 3.23 min

#Variable Importance
Imppls <- varImp( modFitpls_max, scale=F)
plot(Imppls, top=20)
b <- Sys.time();b; b-a


#-----------------------------------------------------------
# Added: 2015-08-21 - 04
#With KNN following example Caret presentation 2011
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitknn <- train(festatus ~ ., data = trainDat,
                   method = "knn",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = cvCtrl)
#Prediction
predknn <- predict(modFitknn, testDat)
#ConfusionMatrix
conMatknn <- confusionMatrix(predknn, testDat$festatus)
#Result: 65% Accuaracy (Specificity: 73% - Sensitivity: 53%)
#Execution time: 1 hour...

#Variable Importance
Impknn <- varImp( modFitknn, scale=F)
plot(Impknn, top=20)


# #-----------------------------------------------------------
# # Added: 2015-08-21 - 05
# #With BARTMACHINE (no reference)
# set.seed(1)
# cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
#                        summaryFunction = twoClassSummary,
#                        classProbs = TRUE, 
#                        verboseIter = FALSE)
# 
# #Model
# modFitbartM <- train(festatus ~ ., data = trainDat,
#                    method = "bartMachine",
#                    tuneLength = 10,
#                    metric = "ROC",
#                    trControl = cvCtrl)
# #Prediction
# predbartM <- predict(modFitbartM, testDat)
# #ConfusionMatrix
# conMat <- confusionMatrix(predbartM, testDat$festatus)
# #Result: % Accuaracy (Specificity: % - Sensitivity: %)
# Gets an execution error....
# 
# #Variable Importance
# ImpbartM <- varImp( modFitbartM, scale=F)
# plot(ImpbartM, top=20)

# #-----------------------------------------------------------
# # Added: 2015-08-21 - 06
# #With LOCLDA (no reference)
# set.seed(1)
# cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
#                        summaryFunction = twoClassSummary,
#                        classProbs = TRUE, 
#                        verboseIter = FALSE)
# 
# #Model
# modFitloclda <- train(festatus ~ ., data = trainDat,
#                    method = "loclda",
#                    tuneLength = 10,
#                    metric = "ROC",
#                    trControl = cvCtrl)
# #Prediction
# predloclda <- predict(modFitloclda, testDat)
# #ConfusionMatrix
# conMat <- confusionMatrix(predloclda, testDat$festatus)
# #Result: % Accuaracy (Specificity: % - Sensitivity: %)
# # Gets an execution error....
# 
# #Variable Importance
# Imploclda <- varImp( modFitloclda, scale=F)
# plot(Imploclda, top=20)
# 


#-----------------------------------------------------------
# Added: 2015-08-21 - 07
#With XGBOOST following example Caret presentation 2011
a <- Sys.time(); a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitxgb <- train(
                   #festatus ~ ., data = trainDat,
                   #festatus ~ ., data = trainDatup,
                   x = trainDat[, 1:11],
                   y = trainDat[,12],
                   method = "xgbTree",
                   tuneLength = 10,
                   metric = "Accuracy",
                   #metric = "ROC",
                   trControl = cvCtrl
                   )
#Prediction
predxgb <- predict(modFitxgb, testDat)
#ConfusionMatrix
conMatxgb <- confusionMatrix(predxgb, testDat$festatus)
#Result: 78% Accuaracy (Specificity: 85% - Sensitivity: 70%)
#Result (with installer): 76% Accuaracy (Specificity: 83% - Sensitivity: 68%)...Worse..??
#Execution time: ~14 minutes... (with installer) 
#-----
#Result 3 Status - 25% sample.
#Result: 76%.
#Execution time: 1.8 hours - 3 status
#-----
#Result 3 Status - 25% sample. - Equally Balanced
#Result: 75%.
#Execution time: 2.25 hours  
#-----
#Result 30 predictors - 3 Status - 25% sample - Equally Balanced
#Result: ~77%.
#Execution time: 12 hours  
#----- 2015-08-07
#Result 11 predictors - 3 Status - 25% sample - not Balanced
#Result:   - Accuracy.
#Execution time: ¡Error!...  
#-----

#Variable Importance
Impxgb <- varImp( modFitxgb, scale=F)
plot(Impxgb, top=20)
b <- Sys.time(); b; b-a

save(
     trainDat, testDat, 
     modFitxgb,
     file="xgb_3class_025_nobalan.RData"
)


#-----------------------------------------------------------
# Added: 2015-09-03 
#With XGBOOST LINEAR 
a <- Sys.time(); a
set.seed(1)
cvCtrl <- trainControl(
                       method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE
                       )

xgbLinGrid = expand.grid( 
  .nrounds = seq(5,10),
  .alpha = 1,
  .lambda = seq(0.01, 0.1, length=5)
)


#Model
modFitxgbLin <- train(
  x = trainDatup[, 1:11],
  y = trainDatup[, 12],
  method = "xgbLinear",
  tuneGrid = xgbLinGrid,
  tuneLength = 10,
  metric = "Accuracy",
  #metric = "ROC",
  trControl = cvCtrl
)
#Prediction
predxgbLin <- predict(modFitxgbLin, testDatup)
#ConfusionMatrix
conMatxgbLin <- confusionMatrix(predxgbLin, testDatup$festatus)
#-----
#Result 11 predictors - all dataset - no balanced
#Result: %.
#Execution time:  hours  
#Results: Constant errors!!. "can not open file "   0"...
#-----
#Result 11 predictors - all dataset - UpBalanced
#Result: %.
#Execution time:  hours  
#Results: Constant errors!!. "Error in train.default..."
#-----

#Variable Importance
ImpxgbLin <- varImp( modFitxgbLin, scale=F)
plot(ImpxgbLin, top=20)
b <- Sys.time(); b; b-a


#-----------------------------------------------------------
# Added: 2015-08-22 - 01
#With EVTREE following example Caret presentation 2011
a <- Sys.time()
a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE)

#Model
#Parameters (evtree vignette: minbucket=10, maxdepth=2 memory requirements increases with the square of maxdepth) 
modFitevtree <- train(festatus ~ ., data = trainDat,
                   method = "evtree",
                   tuneLength = 10,
                   metric = "ROC",
                   minbucket = 10,
                   maxdepth = 2,
                   trControl = cvCtrl)
#Prediction
predevtree <- predict(modFitevtree, testDat)
#ConfusionMatrix
conMatevtree <- confusionMatrix(predevtree, testDat$festatus)
#Result: % Accuaracy (Specificity: % - Sensitivity: %)
#Execution time: (run without minbucket/maxdepth conditions)
#Aborted R session...

#Variable Importance
Impevtree <- varImp( modFitevtree, scale=F)
plot(Impevtree, top=20)
b <- Sys.time()
b-a


#-----------------------------------------------------------
# Added: 2015-08-23 - 01
#With CHAID following example Caret presentation 2011
#http://r-forge.r-project.org/R/?group_id=343
# a <- Sys.time()
# a
# set.seed(1)
# cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
#                        summaryFunction = twoClassSummary,
#                        classProbs = TRUE, 
#                        verboseIter = FALSE)
# 
# #Model
# modFitchaid <- train(festatus ~ ., data = trainDat,
#                       method = "chaid",
#                       tuneLength = 10,
#                       #metric = "ROC",
#                       trControl = cvCtrl)
# #Prediction
# predchaid <- predict(modFitchaid, testDat)
# #ConfusionMatrix
# conMatchaid <- confusionMatrix(predchaid, testDat$festatus)
# #Result: % Accuaracy (Specificity: % - Sensitivity: %)
# #Execution time: Error ... "Something is wrong; all the ROC metric values are missing:"..??
# With all character variables as.factors do not work either...
#
# #Variable Importance
# Impchaid <- varImp( modFitchaid, scale=F)
# plot(Impchaid, top=20)
# b <- Sys.time()
# b-a

#-----------------------------------------------------------
# Added: 2015-08-23 - 02
#With GLM following example Caret presentation 2011
a <- Sys.time()
a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE)

#Model
modFitglm <- train(  
                     #festatus ~ ., data = trainDat,
                     x = trainDat[, 1:11],
                     y = trainDat[, 12],
                     method = "glmboost",
                     tuneLength = 10,
                     #metric = "ROC",
                     metric = "Accuracy",
                     trControl = cvCtrl)
#Prediction
predglm <- predict(modFitglm, testDat)
#ConfusionMatrix
conMatglm <- confusionMatrix(predglm, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 88% - Sensitivity: 57%)
#Execution time: 5 min.

#Variable Importance
Impglm <- varImp( modFitglm, scale=F)
plot(Impglm, top=20)
b <- Sys.time()
b-a


#-----------------------------------------------------------
# Added: 2015-08-23 - 03
#With PLSRGLM following example Caret presentation 2011
a <- Sys.time()
a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE)

#Model
modFitplsrglm <- train(
                   #festatus ~ ., data = trainDat,
                   x = trainDat[, 1:11],
                   y = trainDat[, 12],
                   method = "plsRglm",
                   tuneLength = 10,
                   #metric = "ROC",
                   #metric = "Accuracy",
                   trControl = cvCtrl
                   )
#Prediction
predplsrglm <- predict(modFitplsrglm, testDat)
#ConfusionMatrix
conMatplsrglm <- confusionMatrix(predplsrglm, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 88% - Sensitivity: 57%)
#Execution time: 5 min.
#Result: 12 cols - all set - 3 classes
#Result: Error Accuracy metric are missing...
#Result: Remove metric the same error...

#Variable Importance
Impplsrglm <- varImp( modFitplsrglm, scale=F)
plot(Implm, top=20)
b <- Sys.time()
b-a


#-----------------------------------------------------------
# Added: 2015-08-26 - 01
#With bagEarth
a <- Sys.time(); a
set.seed(1)
cvCtrl <- trainControl(
                       #method = "repeatedcv", repeats = 5,
                       method = "cv", repeats = 3,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE
                       )

#Model
modFitbagear <- train(
                       festatus ~ ., data = trainDat,
                       method = "bagEarth",
                       tuneLength = 10,
                       metric = "ROC",
                       #tuneGrid = data.frame(degree = 1:2),
                       #B=10,
                       trControl = cvCtrl
                       )
#Prediction
predbagear <- predict(modFitbagear, testDat)
#ConfusionMatrix
conMatbagear <- confusionMatrix(predbagear, testDat$festatus)
#Result: 76% Accuaracy (Specificity: 86% - Sensitivity: 64%)
#Execution time: 6 hours with just 15% of data.
#Object: 881Mb! with just "cv" and "3" repetitions.

#Variable Importance
Impbagear <- varImp( modFitbagear, scale=F)
plot(Impbagear, top=20)
b <- Sys.time();b; b-a





#-----------------------------------------------------------
# MODELS - SUMMARY - COMPARISONS
#-----------------------------------------------------------

cvValues <- resamples(
                      list(
                           RF = modFitRF, 
                           GBM = modFitgbm,
                           CART = modFitRpart, 
                           C5.0 = modFitC50,
                           FDA = modFitfda,
                           logistic = modFitplr,
                           lasso = modFitlasso,
                           nnet = modFitnnet,
                           pls = modFitpls,
                           knn = modFitknn,
                           #SVM = modFitsvm,
                           XGB = modFitxgb,
                           glmboost = modFitglm
                          )
                      )
dotplot(cvValues, metric = "ROC")
#Model Correlation
modelCor(cvValues)
summary(cvValues)
summary(cvValues, "ROC")

cvValuesInst <- resamples(
  list(
    LOGISTIC = modFitplr,
    XGB = modFitxgb,
    PLS = modFitpls_max,
    #MDA = modFitmda,
    RF = modFitRF,
    GBM = modFitgbm
  )
)
dotplot(cvValuesInst, metric = "Kappa")
modelCor(cvValuesInst)
summary(cvValuesInst)

#-----------------------------------------------------------
# SAVING MODELS - VARIABLES
#-----------------------------------------------------------

save(
     trainDat, testDat, 
     modFitglm,
     modFitxgb, 
     #modFitsvm,
     modFitknn, modFitpls, modFitgbm, modFitnnet, 
     modFitlasso, modFitRF, modFitC50, modFitfda, 
     modFitplr, modFitRpart, 
     #file="glmboost_svm_xgb_knn_nnet_RF_gbm_C50_fda_plr_Rpart.RData"
     file="xgb_gbm_glm_knn_nnet_RF_gbm_C50_lasso_fda_plr_Rpart_train_test.RData"
    )

save(
     conMatpls_max,
     conMatplr,
     conMatxgb,
     file="conMat_xgb_pls_plr_025_3status_Upscale.RData"
    )
load("conMat_xgb_pls_plr_w-installer.RData")

save(
     trainDatup, testDatup,
     modFitxgb, modFitplr, modFitpls_max, modFitmda, modFitRF, modFitgbm,
     file="xgb_gbm_rf_plr_mda_pls_025_3status_Upscale.RData"
    )
load("xgb_w-installer.RData")

#setwd("/Volumes/STORE N GO/R-cosas/2015-08 - PumpItUp")
#load("svm_xgb_knn_nnet_RF_gbm_C50_fda_plr_Rpart.RData")
#setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp")


#-----------------------------------------------------------
#-----------------------------------------------------------
# TESTING - FILES TO UPLOAD
#-----------------------------------------------------------
#-----------------------------------------------------------
library(data.table)
datTesting <- fread("testing.csv")
# > dim(datTesting)
# [1] 14850    41
#library(stringr)
#insDfori <-  str_to_lower(datTesting$installer)
#datTesting$feinstaller <- insDfori
fehowold <- ifelse(datTesting$construction_year==0,0, 2015-datTesting$construction_year)
datTesting$fehowold <- fehowold
datTesting <- as.data.frame(datTesting)

for( i in 1:ncol(datTesting)) {
  cltmp <- class(datTesting[, i])
  if(cltmp == "character") {
    datTesting[, i] <- as.factor(datTesting[,i])
  } else next
}


#-----------------------------------------------------------
# TESTING - FILES TO UPLOAD - 3 CLASES
#-----------------------------------------------------------
library(stringr)
library(caret)

#C50
#load("c50_3class_025_nobalan.RData")
predConcurso_C50 <- as.vector(predict(modFitC50, newdata=datTesting))
predModify_C50 <- str_replace_all(predConcurso_C50, "_"," ")
#File to updload
fileToUpload_C50 <- data.frame(id=datTesting$id, status_group=predModify_C50)
head(fileToUpload_C50)
write.table(fileToUpload_C50, file="fileToUpload_c50.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#gbm
load("gbm_3class_050_nobalan.RData")
predConcurso_gbm <- as.vector(predict(modFitgbm, newdata=datTesting))
predModify_gbm <- str_replace_all(predConcurso_gbm, "_"," ")
#File to updload
fileToUpload_gbm <- data.frame(id=datTesting$id, status_group=predModify_gbm)
head(fileToUpload_gbm)
write.table(fileToUpload_gbm, file="fileToUpload_gbm.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#xgb
load("xgb_11var_3class_samp100_n100_grid1_nobalan.RData")
predConcurso_xgb <- as.vector(predict(modFitxgbL, newdata=datTesting))
predModify_xgb <- str_replace_all(predConcurso_xgb, "_"," ")
#File to updload
fileToUpload_xgb <- data.frame(id=datTesting$id, status_group=predModify_xgb)
head(fileToUpload_xgb)
write.table(fileToUpload_xgb, file="fileToUpload_xgb.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#----------
#Pendiente.... (11-Sep)
#SVM: 75.22%. (platform: 75.64%)
load("svm_11var_3class_samp050_n10_grid1_nobalan.RData")
predConcurso_svm <- as.vector(predict(modFitsvm, newdata=datTesting))
predModify_svm <- str_replace_all(predConcurso_svm, "_"," ")
#File to updload
fileToUpload_svm <- data.frame(id=datTesting$id, status_group=predModify_svm)
head(fileToUpload_svm)
write.table(fileToUpload_svm, file="fileToUpload_svm3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#rf - 78.9%??
load("rf_11var_3class_samp100_boot200_nobalan.grid1.RData")
noCol <- c('lga')
datTestingfilt <- na.omit(datTesting)
datTestingfilt <- datTestingfilt[ , !(names(datTestingfilt) %in% noCol) ]
predConcurso_rf <- as.vector(predict(modFitrf, newdata=datTestingfilt ))
predModify_rf <- str_replace_all(predConcurso_rf, "_"," ")
#File to updload
fileToUpload_rf <- data.frame(id=datTesting$id, status_group=predModify_rf)
head(fileToUpload_rf)
write.table(fileToUpload_rf, file="fileToUpload_rf.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#xgb - 79.09
load("xgb_19var_3class_samp100_n100_grid1_1_nona_nobalan__79.09__.RData")
#Transform "public_meeting", "permit" in 0 and 1, rather than TRUE/FALSE
datTesting$fepermit <- ifelse(datTesting$permit=="TRUE",1, ifelse(datTesting$permit=="FALSE",0,"NA"))
datTesting$fepubmet <- ifelse(datTesting$public_meeting=="TRUE",1, ifelse(datTesting$public_meeting=="FALSE",0,"NA"))
datTestingRed <- datTesting[, (names(datTesting) %in% names(datInclnNa))]
#missForest considers the last column as predictor and it does not accept NAs.
#put "payment" as the last column
#All columns character -> MUST BE FACTORS!!.
datTestingRed <- datTestingRed[, c(1:11,16:19,12)]
datTestingRednNa <- missForest(datTestingRed, verbose=TRUE)
datTestingRednNag <- datTestingRednNa$ximp
datTestingRednNag$lga <- datTesting$lga
datTestingRednNag$water_quality <- datTesting$water_quality
datTestingRednNag$quality_group <- datTesting$quality_group
datTestingRednNag$quantity <- datTesting$quantity
library(stringr)
datTestingRednNag$lga <- str_replace_all(datTestingRednNag$lga, "Nyamagana","Njombe")
datTesting <- datTestingRednNag
datTesting <- datTesting[, -c(17)]



predConcurso_xgb <- as.vector(predict(modFitxgb, newdata=datTesting))
predModify_xgb <- str_replace_all(predConcurso_xgb, "_"," ")
#File to updload
fileToUpload_xgb <- data.frame(id=datTesting$id, status_group=predModify_xgb)
head(fileToUpload_xgb)
write.table(fileToUpload_xgb, file="fileToUpload_xgb3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#Pendiente.... (11-Sep)
#XGB: R: 79.49% - platform: 0.8007%) WOWWWWW!!!.
load("xgb_17var_3class_samp100_n100_grid1_nobalan.RData")
predConcurso_xgb <- as.vector(predict(modFitxgb, newdata=datTesting))
predModify_xgb <- str_replace_all(predConcurso_xgb, "_"," ")
#File to updload
fileToUpload_xgb <- data.frame(id=datTesting$id, status_group=predModify_xgb)
head(fileToUpload_xgb)
write.table(fileToUpload_xgb, file="fileToUpload_xgb3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#Pendiente.... (11-Sep)
#XGB: R: 79.54% - platform: 0.8003.  Good but not enough to 0.8007.
load("xgb_20var_3class_samp100_n25_gridB_nobalan__79.68__.RData")
predConcurso_xgb <- as.vector(predict(modFitxgb, newdata=datTesting))
predModify_xgb <- str_replace_all(predConcurso_xgb, "_"," ")
#File to updload
fileToUpload_xgb <- data.frame(id=datTesting$id, status_group=predModify_xgb)
head(fileToUpload_xgb); length(predConcurso_xgb)
write.table(fileToUpload_xgb, file="fileToUpload_xgb3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#RF: R: 79.53% - 
load("rf_17var_3class_n100_mtry4_tree200_nobalan__79.53__.RData")
toRemmor <- c('public_meeting','permit','lga')
datTesting <- datTesting[ , !(names(datTesting) %in% toRemmor) ]
namMod <- names(modFitrf$trainingData); namMod <- namMod[1:(length(namMod)-1) ]
datTesting <- datTesting[, (names(datTesting) %in% namMod) ]
predConcurso_rf <- as.vector(predict(modFitrf, newdata=datTesting))
predModify_rf <- str_replace_all(predConcurso_rf, "_"," ")
#File to updload
fileToUpload_rf <- data.frame(id=datTesting$id, status_group=predModify_rf)
head(fileToUpload_rf); length(predConcuso_rf)
write.table(fileToUpload_rf, file="fileToUpload_rf3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

kk <- modFitrf$trainingData
dftmp <- data.frame(kk=0, test=0)
for( i in 1:ncol(kk)) {
  cltmp2 <- class(kk[, i])
  cltmp <- class(datTesting[, i])
  dftmp[i,1] <- names(kk)[i]
  dftmp[i,2] <- cltmp2
  dftmp[i,3] <- cltmp
} 
dftmp
 if(class(kk[,i])=="factor") {
   a <- length(unique(kk[,i]))
   b <- length(unique(datTesting[,i]))
   c <- names(kk)[i]
   print(c(c,a,b))
   kk[,i] <- as.vector(kk[,i])
 } 
}
dftmp
modFitrf$trainingData <- kk

#------------------------------
#RF: R: 79.53% - 
#convert to matrix datTesting...
datTestingredu <- datTesting[, (names(datTesting) %in% names(datIncl))]
datTestingredu$Obj <- sample(c("a","b","c"), nrow(datTestingredu), replace=TRUE)
datTestingredu$Obj <- as.factor(datTestingredu$Obj)
datTestingmat <- data.frame(model.matrix(Obj ~., data=datTestingredu))[,-1]
datTestingsub <- datTestingmat[, (names(datTestingmat) %in% names(datInclmm))]

load("rf_20var_3class_n10_mtry4_100tree_nobalan__79.74__.RData")
predConcurso_rf <- as.vector(predict(modFitrf, newdata=datTesting))
predModify_rf <- str_replace_all(predConcurso_rf, "_"," ")
#File to updload
fileToUpload_rf <- data.frame(id=datTesting$id, status_group=predModify_rf)
head(fileToUpload_rf); length(predConcurso_rf)
write.table(fileToUpload_rf, file="fileToUpload_rf3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#-----------------------------------------------------------
# TESTING - FILES TO UPLOAD - 2 CLASES
#-----------------------------------------------------------
# Models without feinstaller + 2 classes
load("xgb_glm_knn_nnet_RF_gbm_C50_lasso_fda_plr_Rpart_train_test.RData")
#Result: Works!!.
#But: It need to conver bad -> to "not functional" and god -> functional
#The order should be (from low to high):  FDA  -> glmboost -> pls -> lasso -> nnet -> plr -> C5.0 -> RF -> GBM -> XGB <<<<-----
#Lets start with FDA and see what happens
#Important: Check plot(resample) of all models... 

# Models with feinstaller + 3 classes
#load("xgb-3class-upSamp-allpredic.RData")
#Result: Does not work!!. feinstaller in training has new cases.
#The same happens with funder

# Models with feinstaller + 3 classes
#load("xgb_gbm_rf_plr_mda_pls_025_3status_Upscale.RData")
#Result: Does not work!!. feinstaller in training has new cases.

#fda
predConcurso_fda <- as.vector(predict(modFitfda, newdata=datTesting))
predModify_fda <- ifelse(predConcurso_fda =="good", "functional", "non functional")
#File to updload
fileToUpload_fda <- data.frame(id=datTesting$id, status_group=predModify_fda)
head(fileToUpload_fda)
write.table(fileToUpload_fda, file="fileToUpload_fda.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#pls
predConcurso_pls <- as.vector(predict(modFitpls, newdata=datTesting))
predModify_pls <- ifelse(predConcurso_pls =="good", "functional", "non functional")
#File to updload
fileToUpload_pls <- data.frame(id=datTesting$id, status_group=predModify_pls)
head(fileToUpload_pls)
write.table(fileToUpload_pls, file="fileToUpload_pls.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)



#lasso
predConcurso_lasso <- as.vector(predict(modFitlasso, newdata=datTesting))
predModify_lasso <- ifelse(predConcurso_lasso =="good", "functional", "non functional")
#File to updload
fileToUpload_lasso <- data.frame(id=datTesting$id, status_group=predModify_lasso)
head(fileToUpload_lasso)
write.table(fileToUpload_lasso, file="fileToUpload_lasso.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#nnet
predConcurso_nnet <- as.vector(predict(modFitnnet, newdata=datTesting))
predModify_nnet <- ifelse(predConcurso_nnet =="good", "functional", "non functional")
#File to updload
fileToUpload_nnet <- data.frame(id=datTesting$id, status_group=predModify_nnet)
head(fileToUpload_nnet)
write.table(fileToUpload_nnet, file="fileToUpload_nnet.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#plr
predConcurso_plr <- as.vector(predict(modFitplr, newdata=datTesting))
predModify_plr <- ifelse(predConcurso_plr =="good", "functional", "non functional")
#File to updload
fileToUpload_plr <- data.frame(id=datTesting$id, status_group=predModify_plr)
head(fileToUpload_plr)
write.table(fileToUpload_plr, file="fileToUpload_plr.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#C50
predConcurso_C50 <- as.vector(predict(modFitC50, newdata=datTesting))
predModify_C50 <- ifelse(predConcurso_C50 =="good", "functional", "non functional")
#File to updload
fileToUpload_C50 <- data.frame(id=datTesting$id, status_group=predModify_C50)
head(fileToUpload_C50)
write.table(fileToUpload_C50, file="fileToUpload_c50.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#RF
predConcurso_RF <- as.vector(predict(modFitRF, newdata=datTesting))
predModify_RF <- ifelse(predConcurso_RF =="good", "functional", "non functional")
#File to updload
fileToUpload_RF <- data.frame(id=datTesting$id, status_group=predModify_RF)
head(fileToUpload_RF)
write.table(fileToUpload_RF, file="fileToUpload_rf.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#GBM
predConcurso_gbm <- as.vector(predict(modFitgbm, newdata=datTesting))
predModify_gbm <- ifelse(predConcurso_gbm =="good", "functional", "non functional")
#File to updload
fileToUpload_gbm <- data.frame(id=datTesting$id, status_group=predModify_gbm)
head(fileToUpload_gbm)
write.table(fileToUpload_gbm, file="fileToUpload_gbm.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

#XGB
predConcurso_xgb <- as.vector(predict(modFitxgb, newdata=datTesting))
predModify_xgb <- ifelse(predConcurso_xgb =="good", "functional", "non functional")
#File to updload
fileToUpload_xgb <- data.frame(id=datTesting$id, status_group=predModify_xgb)
head(fileToUpload_xgb)
write.table(fileToUpload_xgb, file="fileToUpload_xgb.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#-----------------------------------------------------------
# ENSEMBLES
#-----------------------------------------------------------
# Try pls + knn (correlation of 0.4 )
pred1 <- predict(modFitpls, testDat)
conMatpred1 <- confusionMatrix(pred1, testDat$festatus)
pred2 <- predict(modFitRF, testDat)
conMatpred2 <- confusionMatrix(pred2, testDat$festatus)
predDF <- data.frame(pred1, pred2, festatus=testDat$festatus)
combModFit <- train( festatus ~., method="gam", data=predDF)
combPred <- predict(combModFit, predDF)
conMatcomb <- confusionMatrix(combPred, testDat$festatus)
#Result: Accuracy has always the same value as the higher of the two models...

#What-if: I average "festatus" taking into account all "predModify_XXX", as if they were "votes"...
#It could be all models equal or each model with a weight. Higher weight to better models.
#The order should be (from low to high):  FDA  -> glmboost -> pls -> lasso -> nnet -> plr -> C5.0 -> RF -> GBM -> XGB <<<<-----
allPred <- data.frame(
                      RF=predModify_RF, XGB=predModify_xgb, GBM=predModify_gbm, C50=predModify_C50, 
                      PLR=predModify_plr, PLS=predModify_pls, NNET=predModify_nnet, FDA=predModify_fda,
                      LASSO=predModify_lasso
                      )
allPred <- allPred[, c(8,6,9,7,5,4,3,2,1)]
allPrednum <- ifelse(allPred=="functional", 1,0)
voteEnd <- round(rowMeans(allPrednum),2)
#Weighted mean
myfun <- function(x) {
         wt <- c(1,2,3,4,5,6,7,8,9)/sum(1:9)
         weighted.mean(x,  wt)
} 
voteEndWei <- round(apply(allPrednum, 1, myfun),2)
predWei <- ifelse(voteEndWei < 0.5, "non functional", "functional")
fileToUpload_all <- data.frame(id=datTesting$id, status_group=predWei)
head(fileToUpload_all)
write.table(fileToUpload_all, file="fileToUpload_all.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)



#-----------------------------------------------------------
#-----------------------------------------------------------
# END OF FILE
#-----------------------------------------------------------
#-----------------------------------------------------------



#-----------------------------------------------------------
# Added: 2015-08-20 - 04:
# Lessons Learned: 
# * Blind model searching not suitable for this problem.
# * It is needed a further and deeper study of the variables
# * Some of the variables not needed, and other are equivalent or identical.
# * Start with a much more simplified version of the dataset anb built it up.
# 
# datFac <- datGod
# datFac$region <- as.factor(datFac$region)
# datFac$extraction_type <- as.factor(datFac$extraction_type)
# datFac$management <- as.factor(datFac$management)
# datFac$payment <- as.factor(datFac$payment)
# datFac$quality_group <- as.factor(datFac$quality_group)
# datFac$quantity <- as.factor(datFac$quantity)
# datFac$source <- as.factor(datFac$source)
# datFac$waterpoint_type <- as.factor(datFac$waterpoint_type)
# datFac$feinstaller <- as.factor(datFac$feinstaller)
# 
# chatrain <- datFac[inTrain,]
# chatest <- datFac[-inTrain,]
# modChaid <- chaid( festatus ~., data=chatrain)

#To check "status" the original one with three classes...
datTmp <- datGod
datInclean <- datInclean[ datInclean$extraction_type != "other - mkulima/shinyanga", ]
datTmp$status <- datInclean$status_group
datTmp <- datTmp[, -c(13)]
datTmp$status <- as.factor(datTmp$status)
datGod <- datTmpd
#factor variables recoded without spaces
namtmp <- datTmp$status
namtmp <- str_replace_all(as.vector(namtmp), " ","_")
datGod$status <- as.factor(namtmp)


# Just to see if it is possible to model, a 25%  sample of the whole set.
set.seed(1)
#sizMod <- 0.25 * nrow(datGod)
sizMod <- 1 * nrow(datGod)
datGod <- datGod[sample(1:nrow(datGod), sizMod) , ]


#Although working with the training file, I divide between
#training and testing. Later I will check with the testing file.
inTrain <- createDataPartition(datGod$status, p=0.70)[[1]]
trainDat <- datGod[ inTrain, ]
testDat <- datGod[ -inTrain, ]

#to run parallel computations code does not change just need these three lines
library(doMC) 
numCor <- parallel::detectCores() 
registerDoMC(cores = numCor)



#-----------------------------------------------------------
# Added: 2015-08-27
# With PLS - Three status (original)
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       #summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitplsthree <- train(
                       status ~ ., data = trainDat,
                       method = "pls",
                       tuneLength = 10,
                       #preProc = c("center", "scale", "BoxCox"),
                       metric = "Accuracy",
                       maximize = TRUE,
                       trControl = cvCtrl
                       )
#Prediction
predplsthree <- predict(modFitplsthree, testDat)
#ConfusionMatrix - Three status
conMatplsthree <- confusionMatrix(predplsthree, testDat$status)
#Result: 54% Accuaracy ...
#Execution time: 6 min.

#Variable Importance
Imppls <- varImp( modFitplsthree, value="pls")
plot(Imppls, top=20)
b <- Sys.time();b; b-a


#-----------------------------------------------------------
# Added: 2015-08-20 -02
#With C50 following example Caret-userR2014
a <- Sys.time(); a
set.seed(1)
cvCtrl <- trainControl(
                       method = "repeatedcv", repeats = 2,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)

#C5.0 requires tuning parameters for the boosting.
grid <- expand.grid(model = "tree",
                    trials = 100,
                    winnow = FALSE)
#Model
modFitC50 <- train(
                   festatus ~ ., data = trainDat,
                   method = "C5.0",
                   metric = "ROC",
                   tuneGrid = grid,
                   trControl = cvCtrl
                   )
b <- Sys.time(); b; b-a

#Prediction
predC50 <- predict(modFitC50, newdata=testDat)
#ConfusionMatrix
conMatC50 <- confusionMatrix(predC50, testDat$festatus)
#Result: 77% Accuaracy (Specificity: 68% - Sensitivity: 85%)

#Variable Importance
ImpC50 <- varImp( modFitC50, scale=F)
plot(ImpC50, top=20)



modFitRed <- C5.0( 
  festatus ~. , data=trainDat, trials=100,
  control= C5.0Control(winnow=TRUE)
)

modFitGod <- C5.0( 
  festatus ~. , data=datGod, trials=100,
  control= C5.0Control(winnow=TRUE, sample=0.7)
)

datInclean$status_group <- as.factor(datInclean$status_group)
modFitAll <- C5.0( 
  as.factor(status_group) ~. , data=datInclean[, -c(1,18,20)], trials=100,
  control= C5.0Control(winnow=TRUE, sample=0.7)
)





#-----------------------------------------------------------
# Added: 2015-09-03 
#With xxx (Extreme Learning Machine)
a <- Sys.time(); a
myTrain <- trainDat[trainDat$festatus!="functional_needs_repair",]
myTrain$festatus <- as.factor(as.vector(myTrain$festatus))
myTest <- testDat[testDat$festatus!="functional_needs_repair",]
myTest$festatus <- as.factor(as.vector(myTest$festatus))
set.seed(1)
#  tmp <- createDataPartition(trainDatup$festatus,
#                             p = .8,
#                             times = 100)
cvCtrl <- trainControl(
  #method = "none", 
  #method = "LGOCV", index=tmp,
  method = "repeatedcv", repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE
)

#Model
modFitxxx <- train(
  #x = trainDat[, 1:11],
  #y = trainDat[, 12],
  x = myTrain[,1:11],
  y = myTrain[, 12],
  method = "gpls",
  tuneLength = 10,
  #metric = "Accuracy",
  metric = "ROC",
  #tuneGrid =data.frame(.mtry=3),
  trControl = cvCtrl
)
#Prediction
predxxx <- predict(modFitxxx, myTest)
#ConfusionMatrix
conMatxxx <- confusionMatrix(predxxx, myTest$festatus)
#-----
#Result: 3 classes - upScalling - 25% set.
#Result: Error! in train.default... 
#Execution time: 
#-----
#Result: 3 classes - upScalling - 25% set - LGOCV
#Result: Error! in train.default... 
#Execution time: 

#Variable Importance
Impxxx <- varImp( modFitxxx, scale=F)
plot(Impxxx, top=20)
b <- Sys.time(); b; b-a

#----------
# TO-DO
# gamSpline

#-------------------------------------------------------------------------
#---------------------  ALL MODELS - 2 CLASSES  --------------------------
#-------------------------------------------------------------------------
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
  modFitxxx <- train(x=myTrain[,1:11], y=myTrain[, 12], method=modeLos[i], tuneLength = 10, metric="Accuracy", trControl = cvCtrl )
  
  predxxx <- predict(modFitxxx, myTest)
  conMatxxx <- confusionMatrix(predxxx, myTest$festatus)
  
  b <- Sys.time(); b; b-a
  
  save(conMatxxx, modFitxxx, file=paste("modFit",modelos[i],"_3_class.RData",sep="" ) )
  
}

#------------------------------------------------------------------------------
#------------------ END - ALL MODELS - 3 CLASSES - NOT BALANCED ---------------
#------------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#---------------------  ALL MODELS - 3 CLASSES -  BALANCED -------------------
#-----------------------------------------------------------------------------
modeLos <- c(
  'rf', 'xgbTree', 'nnet',
  'glmboost', 'plsRglm', 'rknn', 'extraTrees','gbm',
  'rpart', 'Boruta', 'RRF', 'gamSpline', 'C5.0Tree', 'C5.0'
)
for(i in 1:length(modeLos)) {
  
  a <- Sys.time(); a
  set.seed(1)
  
  cvCtrl <- trainControl( method = "repeatedcv", repeats = 5, classProbs = TRUE,  verboseIter = FALSE )
  modFitxxx <- train(x=myTrain[,1:11], y=myTrain[, 12], method=modeLos[i], tuneLength = 10, metric="Accuracy", trControl = cvCtrl )
  
  predxxx <- predict(modFitxxx, myTest)
  conMatxxx <- confusionMatrix(predxxx, myTest$festatus)
  
  b <- Sys.time(); b; b-a
  
  save(conMatxxx, modFitxxx, file=paste("modFit",modelos[i],"_3_class.RData",sep="" ) )
  
}

#------------------------------------------------------------------------------
#------------------ END - ALL MODELS - 3 CLASSES - BALANCED -------------------
#------------------------------------------------------------------------------

#-----------------------------------------------------
#------------------ END OF PROGRAM -------------------
#-----------------------------------------------------
