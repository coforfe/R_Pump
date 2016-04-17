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
# As subvillage has 19288 different entries (randomForest, cannot classify more than 32 levels)
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
toOut <- c(1,2,3,4,7,8,9,10,11,13,14,15,16,18,19,20,21,23,24,26,28,29,32,34,35,37,38)
festatus <- ifelse( (datInclean$status_group=="non functional" | datInclean$status_group=="functional needs repair"), "bad", "good")

#---- Data.frame reduced and enhanced...
datGod <- datInclean[, -toOut]
datGod$fehowold <- fehowold
datGod$festatus <- as.factor(festatus)

#---- 2015-08-26 to include installer but in a treated way...(column 6)
#some installers repeated capital o lower letters...
#everything in lower letters.
library(stringr)
insDfori <-  str_to_lower(datGod$installer)
insDf <- as.data.frame(sort(table(insDfori), decreasing=TRUE ))
insDf$Inst <- row.names(insDf)
names(insDf)[1] <- c('Freq')
row.names(insDf) <- NULL
#second installer "empty"..?.
#tenth installer "0"... ?.
dim(insDf)
# [1] 1936    2
# 1936 different installers
#The 50 first installers accounts for the majority of installations...
#plot(insDf$Freq, xlim=c(0,100), ylim=c(0,1000))
#there are some other inconsistences "world", "world bank"...
#"goverment", "goverment central"...
namGod <- insDf$Inst[1:50]
insProc <- ifelse( (insDfori %in% namGod)==TRUE, insDfori, "other")
#now to remove the "" and "0"
insRef <- ifelse( (insProc=="" | insProc=="0"), "unknown", insProc)
#attach to the data.frame
datGod$feinstaller <- insRef
#remove "installer" and sort the columns
datGod <- datGod[, c(1, 3:12,14,13)]

#filter one case in "extraction_type" : "other - mkulima/shinyanga"
#just two records and both are "bad"...
#the existence of this creates problems in PLS y PLR models.
datGod <- datGod[ datGod$extraction_type!="other - mkulima/shinyanga",]

#datGod$fecuthowold <- fecuthowold (not included initially)
#-----------------------------------------------------------------
dim(datGod)
# 59398    13
head(datGod)
save(
      datGod,
      file="datGod.RData"
)
#-----------------------------------------------------------------

# Just to see if it is possible to model, a 25%  sample of the whole set.
set.seed(1)
#sizMod <- 0.15 * nrow(datGod)
sizMod <- 1 * nrow(datGod)
datGod <- datGod[sample(1:nrow(datGod), sizMod) , ]


#Although working with the training file, I divide between
#training and testing. Later I will check with the testing file.
inTrain <- createDataPartition(datGod$festatus, p=0.70)[[1]]
trainDat <- datGod[ inTrain, ]
testDat <- datGod[ -inTrain, ]

#to run parallel computations code does not change just need these three lines
library(doMC) 
numCor <- parallel::detectCores() 
registerDoMC(cores = numCor)


#----------------------------------------------------
# DATA MODEL
#----------------------------------------------------

#-----------------------------------------------------------
#With RF following example Caret-userR2014
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)
#Model
modFitRF <- train( festatus ~. , data=trainDat, 
                   method = "rf",
                   metric = "ROC",
                   tuneLength = 10,
                   trControl=cvCtrl )
#Prediction
predrf <- predict(modFitRF, newdata=testDat)

#ConfusionMatrix
conMatrf <- confusionMatrix(testDat$festatus, predrf)
#Result: 78% Accuaracy (Specificity: 77% - Sensitivity: 80%)
#Execution time: ~5 hours.
#modFitRF object in workspace occupies 40Mb

#Variable Importance
Imprf <- varImp( modFitRF, scale=F)
plot(Imprf, top=20)
 
#-----------------------------------------------------------
#With GBM following example Caret-userR2014
#gbmGrid de JSS paper.
#gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
#                        .n.trees = (1:10)*25, .shrinkage = .1)
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)
#Model
modFitgbm <- train( festatus ~. , data=trainDat,
                    method="gbm",
                    metric = "ROC",
                    tuneLength = 10,
                    trControl=cvCtrl
                   )

#Prediction
predgbm <- predict(modFitgbm, newdata=testDat)
#ConfusionMatrix
conMatgbm <- confusionMatrix(testDat$festatus, predgbm)
#Result: 78% Accuaracy (Specificity: 78% - Sensitivity: 78%)

#Variable Importance
Impgbm <- varImp( modFitgbm, scale=F)
plot(Impgbm, top=20)

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
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)

#C5.0 requires tuning parameters for the boosting.
grid <- expand.grid(model = "tree",
                    trials = 1:100,
                    winnow = FALSE)
#Model
modFitC50 <- train(festatus ~ ., data = trainDat,
                     method = "C5.0",
                     metric = "ROC",
                     tuneGrid = grid,
                     trControl = cvCtrl)
#Prediction
predC50 <- predict(modFitC50, newdata=testDat)
#ConfusionMatrix
conMatC50 <- confusionMatrix(predC50, testDat$festatus)
#Result: 77% Accuaracy (Specificity: 68% - Sensitivity: 85%)

#Variable Importance
ImpC50 <- varImp( modFitC50, scale=F)
plot(ImpC50, top=20)


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
# Added: 2015-08-20 - 06
#With PLR (logistic) following example Caret-userR2014
a <- Sys.time();a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = FALSE)

modFitplr <- train(festatus ~ ., data = trainDat,
                   method = "multinom",
                   tuneGrid = data.frame(decay = c(0.1, 1, 10, 20, 40)),
                   trace = FALSE, maxit = 1000,
                   metric = "ROC",
                   trControl = cvCtrl)
#Prediction
predplr <- predict(modFitplr, testDat)
#ConfusionMatrix
conMatplr <- confusionMatrix(predplr, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 86% - Sensitivity: 64%)
#Result (with installer): 74% Accuaracy (Specificity: 85% - Sensitivity: 61%)...Worse..??
#Execution time: ~ 4 min. (25% sampled - w-installer)
#Execution time: ~ 23 min. (100% sampled - w-installer)

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
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitpls <- train(festatus ~ ., data = trainDat,
                    method = "pls",
                    tuneLength = 10,
                    #preProc = c("center", "scale", "BoxCox"),
                    metric = "ROC",
                    maximize = TRUE,
                    trControl = cvCtrl)
#Prediction
predpls <- predict(modFitpls, testDat)
#ConfusionMatrix
conMatpls <- confusionMatrix(predpls, testDat$festatus)
#Result: 76% Accuaracy (Specificity: 87% - Sensitivity: 62%)
#Result (with installer): 74% Accuaracy (Specificity: 87% - Sensitivity: 60%)...Worse..??
#Execution Time: 1.59 mins!!.
#Execution Time (whole dataset):
#Result (with installer): 74% Accuaracy (Specificity: 87% - Sensitivity: 60%)...Worse..??
#Result (with installer + maximize=TRUE): 75% Accuaracy (Specificity: 88% - Sensitivity: 60%)...Worse..??

#Variable Importance
Imppls <- varImp( modFitpls, scale=F)
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


#-----------------------------------------------------------
# Added: 2015-08-21 - 07
#With XGBOOST following example Caret presentation 2011
a <- Sys.time(); a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = FALSE)

#Model
modFitxgb <- train(festatus ~ ., data = trainDat,
                   method = "xgbTree",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = cvCtrl)
#Prediction
predxgb <- predict(modFitxgb, testDat)
#ConfusionMatrix
conMatxgb <- confusionMatrix(predxgb, testDat$festatus)
#Result: 78% Accuaracy (Specificity: 85% - Sensitivity: 70%)
#Result (with installer): 76% Accuaracy (Specificity: 83% - Sensitivity: 68%)...Worse..??
#Execution time: ~14 minutes... (with installer) 

#Variable Importance
Impxgb <- varImp( modFitxgb, scale=F)
plot(Impxgb, top=20)
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
# Added: 2015-08-23 - 02
#With GLM following example Caret presentation 2011
a <- Sys.time()
a
set.seed(1)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE)

#Model
modFitglm <- train(festatus ~ ., data = trainDat,
                     method = "glmboost",
                     tuneLength = 10,
                     metric = "ROC",
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
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE, 
                       verboseIter = TRUE)

#Model
modFitplsrglm <- train(festatus ~ ., data = trainDat,
                   method = "plsRglm",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = cvCtrl)
#Prediction
predplsrglm <- predict(modFitplsrglm, testDat)
#ConfusionMatrix
conMatplsrglm <- confusionMatrix(predplsrglm, testDat$festatus)
#Result: 75% Accuaracy (Specificity: 88% - Sensitivity: 57%)
#Execution time: 5 min.

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
                       method = "repeatedcv", repeats = 5,
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
#Result: % Accuaracy (Specificity: % - Sensitivity: %)
#Execution time:  min.

#Variable Importance
Impbagear <- varImp( modFitbagear, scale=F)
plot(Implm, top=20)
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
                           SVM = modFitsvm,
                           XGB = modFitxgb,
                           glmboost = modFitglm,
	                   evtree = modFitevtree,
                           bagearth = modFitbagear,
                           plsRglm = modFitplsrglm
                          )
                      )
dotplot(cvValues, metric = "ROC")


#-----------------------------------------------------------
# SAVING MODELS - VARIABLES
#-----------------------------------------------------------

save(
     trainDat, testDat, 
     modFitevtree, modFitbagear, modFitplsrglm,
     modFitglm,
     modFitxgb, 
     modFitsvm,
     modFitknn, modFitpls, modFitgbm, modFitnnet, 
     modFitlasso, modFitRF, modFitC50, modFitfda, 
     modFitplr, modFitRpart, 
     file="evtree_bagear_plsrglm_xgb_svm_glm_knn_nnet_RF_gbm_C50_lasso_fda_plr_Rpart_train_test.RData"
    )


#-----------------------------------------------------------
# TO-DO
#-----------------------------------------------------------
#

#-----------------------------------------------------------
# END OF FILE
#-----------------------------------------------------------
