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

#----------------------------------------------------
# DATA LOADING
#----------------------------------------------------
#Load training  and trainingStatus data 
library(data.table)
datTrain <- fread("training.csv")
datStat <- fread("trainingStatus.csv")
datIn <- datTrain[ , status_group:=datStat$status_group]
datIn <- as.data.frame(datIn)

datTesting <- fread("testing.csv")
datTesting <- as.data.frame(datTesting)


#----------------------------------------------------
# DATA MUNGING - CLEANING
#----------------------------------------------------
# clean_data.R -> 
# Removed:
# wpt_name, num_private, subvillage, region_code, district_code, lga, ward, recorded_by, scheme_name, 
# Modified: 
# funder: everything is named "Other"
# Installer: Everything is named "Other"
# Construction year: factor and reduced to factor. "0" become "y0".
# extraction_type: "mkulima/shinyanga" changed to "other" in test.csv.
# New: 
# date_recorded_offset_days: Days operating
# date_recorded_month: Months operating
#
#Remove columns with more values in the "training.csv" file.
#And others...repeated.
library(stringr)
#Inconsistent:  'funder','installer','wpt_name', 'subvillage','ward','scheme_name',
# toRem <- c(
#            'id',
#            'funder','installer','wpt_name', 'subvillage','ward','scheme_name',
#            'longitude', 'latitude','date_recorded', 'water_quality', 'region_code',
#            'district_code','recorded_by','extraction_type_group', 'extraction_type_class',
#            'management_group','payment_type','quantity_group','source_type',
#            'source_class', 'waterpoint_type_group')
# datIncl <- datIn[, !(names(datIn) %in% toRem) ]
#toRemNew: to include in the model: water_quality, region_code, district_code
toRemNew <- c(
    'id',
    'funder','installer','wpt_name', 'subvillage','ward','scheme_name',
    'longitude', 'latitude','date_recorded', 
    'recorded_by','extraction_type_group', 'extraction_type_class',
    'management_group','payment_type','quantity_group','source_type',
    'source_class', 'waterpoint_type_group') 
datIncl <- datIn[, !(names(datIn) %in% toRemNew) ]
#Transform "public_meeting", "permit" in 0 and 1, rather than TRUE/FALSE
#datIncl$fepermit <- ifelse(datIncl$permit=="TRUE",1, ifelse(datIncl$permit=="FALSE",0,"NA"))
#datIncl$fepubmet <- ifelse(datIncl$public_meeting=="TRUE",1, ifelse(datIncl$public_meeting=="FALSE",0,"NA"))
toRemmor <- c('public_meeting','permit')
datIncl <- datIncl[ , !(names(datIncl) %in% toRemmor) ]
#To creante how old and new status
datIncl$fehowold <- ifelse(datIncl$construction_year==0,0, 2015-datIncl$construction_year)
datIncl$festatus <- str_replace_all(datIncl$status_group," ","_")
toRemnew <- c('construction_year', 'status_group')
datIncl <- datIncl[ , !(names(datIncl) %in% toRemnew) ]

#To transform in factors the character columns. It's very convenient.
for( i in 1:ncol(datIncl)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "character") {
    datIncl[, i] <- as.factor(datIncl[,i])
  } else next
}

#Save clean data.frame
save( datIncl, file="datIncl.RData" )

# > dim(datIncl)
# [1] 59400    19
# 18 predictors + 1 objective

# #Permit and Public NA filling... using library(missForest)
# # datIncl - datTesting
# library(missForest)
# #for randomForest to remove "lga"
# datInclnNa <- missForest(datIncl[, -c(8)], verbose=TRUE)
# datInclnNa <- datInclnNa$ximp
# datIncl <- datInclnNa
# #Although I removed "lga" for the missForest, I put it again...
# datIncl$lga <- datIn$lga
# datIncl <- datIncl[, c(1:20, 22, 21)] #reorder
# 


# # #Do that automatically when colums are present
# if(sum(names(datIncl)=="fepubmet" | names(datIncl)=="fepermit") > 0) {
#   datIncl <- na.omit(datIncl)
# } 

# #With model.matrix... to test with RF...
# datInclmm <- data.frame(model.matrix(festatus ~. , data=datIncl))[,-1]
# datInclmm$festatus <- datIncl$festatus
# datSamp <- datInclmm
#   
#----------------------------------------------------
# TRAIN AND TEST SETS
#----------------------------------------------------
library(data.table)
library(stringr)
datIncl <- fread("myTrain.csv")
datIncl <- as.data.frame(datIncl)
names(datIncl)[31] <- c('festatus')
datIncl <- datIncl[, c(1:30, 32:33,31)] #reorder data.frame
datIncl$festatus <- as.factor(datIncl$festatus)
datIncl <- datIncl[, -c(11,13)] #remove permit and  public_meeting.
datIncl$festatus <- str_replace_all(datIncl$festatus, " ", "_")
datIncl <- datIncl[, -c(3,5)] #remove funder, installer, if not it does not work... It cannot be all equal.

veodf <- data.frame()
cont <- 0
for( i in 1:ncol(datIncl)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "factor") {
    cont <- cont+1
    veodf[i, 1] <- names(datIncl)[i]
    veodf[i, 2] <- length(levels(datIncl[,i]))
  } else { 
    veodf[i,1] <- names(datIncl[i])
    veodf[i,2] <- 0
    }
}

library(caret)
set.seed(1)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]

#-------------- upSampling
# Just to see if it is possible to model, a 25%  sample of the whole set.
datUp <- upSample(datIncl[,1:16], datIncl[,17], yname="festatus") #for toOut - no installer
set.seed(1)
#sizModup <- 0.25 * nrow(datUp)
sizModup <- 1 * nrow(datUp)
datSampUp <- datUp[sample(1:nrow(datUp), sizModup) , ]

inTrainup <- createDataPartition(datSampUp$festatus, p=0.70)[[1]]
trainDatup <- datSampUp[ inTrainup, ]
testDatup <- datSampUp[ -inTrainup, ]



#----------------------------------------------------
#----------------------------------------------------

#----------------------------------------------------
# MODELS
#----------------------------------------------------

#--------------------------------- 
#---------------------- XGB Linear
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=150, verboseIter=TRUE) 

#   xgbGridL <- expand.grid(
#     .nrounds = seq(100, 350, 50),
#     .lambda = seq(0.1, 0.6, 0.15),
#     .alpha = seq(0.1, 0.5, 0.15)
#   )

xgbGridL <- expand.grid(
  .nrounds = 100,
  .lambda = 0.4,
  .alpha = 0.25
)


modFitxgbL <-  train(
  festatus ~ .,
  data = trainDat,
  #x = trainDat[,1:11],
  #y = trainDat[,12],
  method = "xgbLinear",
  trControl = bootControl,
  num_class = 3,
  tuneGrid = xgbGridL
)

predxgbL <- predict(modFitxgbL, newdata=testDat[,1:16])
#ConfusionMatrix
conMatxgbL <- confusionMatrix(testDat$festatus, predxgbL) 
b <- Sys.time();b; b-a   

#Chart about the simulation
if( nrow(xgbGridL) < 2  )  { resampleHist(modFitxgbL) } else 
{ plot(modFitxgbL, as.table=T) }

#Save model and datasets train/test.
save(
  trainDat, testDat, modFitxgbL,
  file="xgbL_16var_3class_100_n150_grid1_nobalan.RData"
)




#---------
#Data: 18 variables - 3 classes - 040 sample - not balanced
#Expe: Boot: 3 - tuneGrid: 72 vars . Best: alpha:0.25 - nrounds:100 - lambda:0.4
#Rest: 76.4%.
#ExTm: 1.02hours
#---------
#Data: 18 variables - 3 classes - 040 sample - not balanced
#Expe: Boot: 100 - tuneGrid: 3 vars . Best: alpha:0.25 - nrounds:100 - lambda:0.4
#Rest: 77%.
#Note:permit + public_meeting has many NAs. Later is impossible to get the confusionMatrix.
#Note:lets remove them.
#ExTm: 9.79mins
#---------
#Data: **16** variables - 3 classes - 100 sample - not balanced
#Remove: permit - public_meeting.
#Expe: Boot: 150 - tuneGrid: 3 vars . Best: alpha:0.25 - nrounds:100 - lambda:0.4
#Rest: 77.9%.
#ExTm: 2.56mins (run parallel)
#---------
#Data: **16** variables - 3 classes - 100 sample - not balanced
#Remove: permit - public_meeting.
#Expe: Boot: 150 - tuneGrid: 3 vars . Best: alpha:0.25 - nrounds:100 - lambda:0.4
#Rest: 77.9%.
#ExTm: 7.41mins (run parallel)



#--------------------------------- 
#---------------------- RF
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

#Remove 'lga' column
noCol <- c('lga')
trainDat <- trainDat[, !(names(trainDat) %in% noCol) ]
testDat <- testDat[, !(names(testDat) %in% noCol) ]

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5) 

#    rfGrid <- expand.grid(
#      .mtry = seq(20, 200, length.out=5 )
#    )
 rfGrid <- expand.grid(.mtry=seq(20,25,1))


modFitrf <-  train(
        festatus ~ .,
        data = trainDat,
  method = "rf",
  trControl = bootControl,
  tuneGrid = rfGrid,
  do.trace = TRUE,
  ntree =  200
)

predrf <- predict( modFitrf, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatrf <- confusionMatrix(testDat$festatus, predrf); conMatrf 
conMatrfdf <- as.data.frame(conMatrf$overall); rfAcc <- conMatrfdf[1,1]; rfAcc <- as.character(round(rfAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(rfGrid) < 2  )  { resampleHist(modFitrf) } else  { plot(modFitrf, as.table=T) }

#Accuracy:
rfAcc
#Best iteration
modBest <- modFitrf$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitrf$times$final[3]
#Samples
samp <- dim(modFitrf$resample)[1]
numvars <- ncol(trainDat)

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitrf,
  #file=paste("rf_20var_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",rfAcc,"__.RData", sep="")
  file=paste("concurso_",numvars,"vars_rf_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",rfAcc,"__.RData", sep="")
)


# save(
#   trainDat, testDat, modFitrf,
#   file=paste("rf_modmat_form_20var_3class_n10_mtry65_200tree_nobalan__",rfAcc,"__.RData", sep="")
# )


#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid : mtry: 25 and 30 (all predictors=28)
#Summ: Stops growing. 
#Rest: 80.31%.
#ExTm: 49.78 mins.

#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid : mtry: 20 and 25 - Best 25. 
#Summ: It can grow a little bit more.
#Rest: 80.53%.
#ExTm: 59.30  mins.

#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid : mtry: 15 and 20 - Best 20. 
#Summ: It can grow a little bit more.
#Rest: 80.3%.
#ExTm: 41.45  mins.
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid : mtry:8:12 - 
#Summ: It continues growing. 
#Rest: 79.53%.
#ExTm: 1.47 hours


#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid : mtry:5:8 - Best: 8.
#Summ: It can grow with higher mtry.
#Rest: 78.28%.
#ExTm: 1.21 hours
#---------
#--------- Data clean and process with a R program in the virtual space.
#---------
#---------

#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, Grid mtry: 17 and ntrees:200
#Summ: No model.matrix. Original set.
#Rest:  79.66%. 
#ExTm:   27.66mins

#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, Grid mtry: 65
#Summ: With Model Matrix: 65 best value. More boostraps does not change.
#Rest:  79.15%. 
#ExTm:  38.77 mins
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid mtry: 50:150
#Summ: With Model Matrix: 65 best value. 
#Rest:  79.15%. 
#ExTm:  3.87 hours
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, Grid fixed: mtry:17 - ntrees:100
#Summ: With Model Matrix: 237 columns... : Lower value but, needs to study with a Grid. 
#Rest:  78.21%. 
#ExTm:  33mins
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, Grid fixed: mtry:17 - ntrees:100
#Summ: It improves...
#Rest:  79.66%. 
#ExTm:  12mins
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid fixed: mtry:17 - ntrees:150
#Summ: Reduce ntrees to 100??
#Rest:  79.52%%. 
#ExTm: 36 ins
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid mtry:12-16 - Best: mtry: 17 - ntree = *150*
#Summ:  It improves!. Saturates at mtry:17. !!Good!!.
#Rest:  79.65%%. 
#ExTm:  mins
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid mtry:9-12 - Best: mtry: 12 
#Summ: It continues improving... More mtry!... No sense mtry=number predictors??.
#Rest:  79.03%. 
#ExTm:  26.97mins
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid mtry:6-9 - Best: mtry: 9
#Summ: Higher mtry.!!.
#Rest:  77.77%. 
#ExTm:  21mins
#---------
#New extended dataset, with three new columns and no permit/public...
#water_quality has more levels than quality_group, but very equivalent
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid mtry:5-6 - Best: mtry:6
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Rest:  76.17%.  - Good improvement!!.
#ExTm:  13.43mins
#---------
#Care: USE FORMULA!!. Not matrix...
#New extended dataset, with three new columns and no permit/public...
#water_quality has more levels than quality_group, but very equivalent
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, Grid mtry:2-4 - Best: mtry:4
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Rest:  73.87%. 
#ExTm:  17.8mins.  


#--------
# With formula: 0.7395 (10.61 mins). It works!!. Predicts...
# Separated: 0.7974 (6.67 mins). Don't predict!!!.

#---------
#New extended dataset, with three new columns and no permit/public...
#water_quality has more levels than quality_group, but very equivalent
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, mtry=4
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Note: Just a trial. Grid should be extensive...
#Rest:  79.74%. 
#ExTm:  6.7mins.  
#Note: Test if it predicts!!. Make models with data *NOT* transformed in factors!!.

#---------
#Data: 16 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 150, tuneGrid: mtry=2 + ntree=100 (not 500 default)
#Rest: %. - Error!. Some of the new variables have more than 32 levels (lga)....
#ExTm: hours
#---------
#Data: **15** variables - 3 classes - 100 sample - not balanced
#Note: 'lag' removed.
#Expe: bootstrap: 150, tuneGrid: mtry=2 + ntree=100 (not 500 default)
#Rest: 79.11%. 
#ExTm:  42.47 mins
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 50, tuneGrid: mtry=2 + ntree=100 (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.32%. 
#ExTm:  15mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: 50, tuneGrid: mtry=2 + ntree=100 (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Note: with parameter "maximize"=TRUE.
#Rest:  79.32%. 
#Summ: No change..
#ExTm:  17.6mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *250*, tuneGrid: mtry=2 + ntree=100 (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.55%. 
#Summ: A little better...
#ExTm:  1.25 hours.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *10*, tuneGrid: mtry=2 + ntree=*200* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.61%. 
#Summ: A little better... more trees??
#ExTm:  8.14 mins.
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *10*, tuneGrid: mtry=2 + ntree=*300* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.54%. 
#Summ: A little lower...
#ExTm:  10mins.
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *50*, tuneGrid: mtry=2 + ntree=*200* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.48%. 
#Summ: It does not improve with the number of bootstraps.
#ExTm:  31mins.
---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *10*, tuneGrid: mtry=2 + ntree=*250* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.56%. 
#Summ: No change for ntree 250.. What if ntree=600??
#ExTm: 8.3mins.
  ---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *25*, tuneGrid: mtry=2 + ntree=*600* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest: 79.53%. 
#Summ: No change...
#ExTm: 42mins.
---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *25*, tuneGrid: mtry=*3* + ntree=*100* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest: 79.78%. 
#Summ: it improves!!. 
#ExTm: 12mins.
---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *25*, tuneGrid: mtry=*3* + ntree=*200* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest: 79.89%. 
#Summ: Continues improving... (more trees?)
#ExTm: 23mins.
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *25*, tuneGrid: mtry=*3* + ntree=*300* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest: 79.88%. 
#Summ: No more trees,but more mtry?.. 
#ExTm: 38 mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced - No NAs
#Expe: bootstrap: *25*, tuneGrid: mtry=*4* + ntree=*200* (not 500 default)
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest: 79.63%. 
#Summ: Nope... enough... 
#ExTm: 38 mins.  
#--------- Back to 15 vars.
#Data: *15* variables - 3 classes - 100 sample - not balanced  
#Expe: bootstrap: *25*, tuneGrid: mtry=*4* + ntree=*200* (not 500 default)
#Note: No "permit", no "pubic"... (ref: 79.49% of xgb)
#Rest: 79.53%. 
#Summ: it improves values of mtry=2 y ntree=100, and better than xgb!. 
#ExTm:  38mins.    
#--------- 
#Data: *15* variables - 3 classes - 100 sample - not balanced  
#Expe: bootstrap: *25*, tuneGrid: mtry=*5* + ntree=*200* (not 500 default)
#Note: No "permit", no "pubic"... (ref: 79.49% of xgb)
#Rest: 79.27%. 
#Summ: it gets worse...
#ExTm:  47mins.      
    

#--------------------------------- 
#---------------------- XGB
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

trainDat <- trainDatup
testDat <- testDatup

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

#   xgbGrid <- expand.grid(
#                          .eta = seq(0.09, 0.12, 0.01),
#                          .max_depth = seq(4,12,2),
#                          .nrounds = seq(300, 400,50)
#                          )

  xgbGrid <- expand.grid(
                         .eta = seq(0.14, 0.15, 0.01),
                         .max_depth = 13,
                         .nrounds = seq(600, 700,100 )
                         )

# xgbGrid <- expand.grid(
#   .eta = 0.11,
#   .max_depth = 10,
#   .nrounds = 300
# ) 


modFitxgb <-  train(
  festatus ~ .,
  data = trainDat,
  method = "xgbTree",
  trControl = bootControl,
  verbose = 1,
  num_class = 3,
  tuneGrid = xgbGrid
)

predxgb <- predict( modFitxgb, newdata=testDat[,1:(ncol(testDat)-1)] )
#predxgb <- predict( modFitxgb, newdata=testDat[,1:19] )
#http://topepo.github.io/caret/preprocess.html#impute
#http://stackoverflow.com/questions/20054906/carettrain-values-not-imputed
#predxgb <- predict(modFitxgb, newdata=testDat[,1:18], na.action=na.pass)
#ConfusionMatrix
conMatxgb <- confusionMatrix(testDat$festatus, predxgb); conMatxgb 
conMatxgbdf <- as.data.frame(conMatxgb$overall); xgbAcc <- conMatxgbdf[1,1]; xgbAcc <- as.character(round(xgbAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(xgbGrid) < 2  )  { resampleHist(modFitxgb) } else  
{ plot(modFitxgb, as.table=T) }

# #Variable Importance
# Impxgb <- varImp( modFitxgb, scale=F)
# plot(Impxgb, top=20)

#Best iteration
modBest <- modFitxgb$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitxgb$times$final[3]
#Samples
samp <- dim(modFitxgb$resample)[1]


save(
  trainDat, testDat, modFitxgb,
  file=paste("xgb_17var_3class_samp100_n",samp,"_grid",modBestc,"_Upbalan__",xgbAcc,"__.RData", sep="")
)

#Note: Include pop but by Subvillage (that was removed)...?

#--- New variables : dist + numbpump region + people/pump 
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 1 - Best: 
#Summ: Increase max_depth - No improvement...
#Rest: 80.59%.
#ExTm: 1.32hours

#--- New variables : dist + numbpump region + people/pump 
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 1 - Best: 300, 13, 0.11.
#Summ: It seems it can grow with max_depth
#Rest: 80.59%%.
#ExTm: 12min

#--- New variables : dist + numbpump region + people/pump 
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 4 - Best:
#Summ: Or <300, 12, 0.11? Nope..
#Rest: 80.49%%.
#ExTm: 17.53min

#--- New variables : dist + numbpump region + people/pump 
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 8 - Best:
#Summ: Does it improve in a grid?. Nope... A little bit: >400, 0.12, 12
#Summ: Or <300, 12, 0.11
#Rest: 80.49%.
#ExTm: 49.80min

#--- New variables : dist + numbpump region + people/pump 
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 1 - 300 - 10 - 0.11
#Summ: It improves a little...
#Rest: 80.55%.
#ExTm: 8min

#--- Adding a sqrt(x^2 + y^2)
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 1 - 300 - 10 - 0.11
#Summ: Let's see if it improves in a grid...
#Rest: 80.52%.
#ExTm: 11min
#--- Adding a x^2 + y^2
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 1 - 300 - 10 - 0.11
#Summ: 
#Rest: 80.52%.
#ExTm: 10.min
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 6 - 200 - 12 - 0.1
#Summ: Satura a 12 max_tree.
#Rest: 80.24%.
#ExTm: 1.28hours
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 12 - Best:  300 - 9 - 0.1
#Summ: it could improve with 200, 0.10 and more max_tree
#Rest: 80.14%.
#ExTm: 1.32hours
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 9 - Best:  500 - 14 - 0.12
#Summ: It does not improve more...
#Rest: 80.41%.
#ExTm: 1.10hours
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 18 - Best:  400 - 0.11 - 10
#Rest: 80.17%.
#ExTm: 1.42hours
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 1: - 300 - 0.11 - 10
#Rest: 80.74%.
#ExTm: 15mins
#---------
#--------- Data clean and process with a R program in the virtual space.
#---------



#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 1 - 400 - 0.12 - 10
#Note:  - no permit/public... 
#Sum: Just to get a model for the platform (with a expected value something higher than 0.8) 
#Rest: 82.89%. 
#ExTm: 20.51mins.  


#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid 1 - 400 - 0.12 - 9
#Note:  - no permit/public... 
#Sum: Just to get a model for the platform (with a expected value something higher than 0.8) 
#Rest: 81.9%. 
#ExTm: 18.43mins.  


#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 1 - Best: 900 - 0.15 - 13
#Note:  - no permit/public... 
#Sum:  It gets saturated. max_depth: 13 and  shrinkage 0.15 (fixed!). improves a little with number of iterations...
#Rest: 84.75%. 
#ExTm: 58.30 mins.  

#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 4 - Best: 800 - 0.15 - 13
#Note:  - no permit/public... 
#Sum:  It gets saturated. max_depth: 13 (fixed!). END! - Reached the maximum...!!. - Only improvement in the number of iterations.
#Rest: 84.73%. 
#ExTm: 58.30 mins.  

#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 8 - Best: 700 - 0.15 - 13
#Note:  -  no permit/public... 
#Sum:  It gets saturated. max_depth: 13 (fixed!). It imporves a little bit..
#Rest:  84.76%. 
#ExTm:  48 mins.


#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 8 - Best: 600 - 0.14 - 13
#Note:  -  no permit/public... 
#Sum:  It gets saturated. max_depth: 13 (fixed!). It can improve with nrounds and eta.
#Rest:  84.70%. 
#ExTm:  1.27 hours.

#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 8 - Best: 500 - 0.13 - 13
#Note: upSampling?? -  no permit/public... 
#Sum:  It can grow even more...
#Rest:  84.69%. 
#ExTm:  1.075 hours.

#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 60 - Best: 
#Note: upSampling?? -  no permit/public... 
#Sum: El mejor resultado de siempre!!!!! GOOOOODDDD!!!!. (VarImp takes a lot to process)
#Sum: Parece que puede seguir creciendo.
#Rest:  83.99%. 
#ExTm:  160.4mins.

#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *25*, tuneGrid 1.  - nrounds: 300 - max_tree: 10 - shrink: 0.11
#Note: no permit/public... 
#Sum: It does not improve with more samples. 
#Rest:  79.49%. 
#ExTm:  15mins.
#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid length 60.  - nrounds: 300 - max_tree: 10 - shrink: 0.11
#Note: no permit/public... 
#Sum: It's a little lower than with more variables.
#Rest:  79.49%. 
#ExTm:  59.6mins.
#
#---------
#--------- Back to 17 variables!!!
#---------
#Data: *22* variables - permit/public noNA imputed - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid length 60.  - 
#Note: Included: region_code, extraction_code, water_quality + permit + public..
#Sum: 
#Rest:  79.66%. 
#ExTm:  1.18 hours.
#---------
#Data: *22* variables - permit/public noNA imputed - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid length 60.  - Best: nrounds=350, max_depth=8, eta=0.11
#Note: Included: region_code, extraction_code, water_quality. 
#Sum: Due to an error, fepermit and fepubmet had NAs.. ??. Run again.. 
#Rest:  78.86%. 
#ExTm:  68 mins.

#---------
#--------- Now 22 variables - Including permit / public - imputed with missForest..
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *25*, tuneGrid length 8. - Best: max_tree: 13 - shrink: 0.12 - nrounds:100
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Sum: The first set with a higher value 79.68. Achieved!.
#Rest:  79.68%. 
#ExTm:  8.24mins.  
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *25*, tuneGrid length 8. - Best: max_tree: 13 - shrink: 0.15 - nrounds:90
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Sum: With more samples no change...
#Rest:  79.61%. 
#ExTm:  7mins.  
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid length 8. - Best: max_tree: 13 - shrink: 0.15 - nrounds:90
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Sum: Get worse...
#Rest:  79.61%. 
#ExTm:  5mins.  
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid length 8. Best: max_tree: 13. shrink:0.12. nrounds:100  
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Rest:  79.68%. 
#ExTm:  11mins.  
#---------
#---------
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid length 8. Best: max_tree: 12. shrink:0.10. nrounds:200  
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Rest:  79.62%. 
#ExTm: 18 mins.  
#---------
#New extended dataset, with three new columns and no permit/public...
#water_quality has more levels than quality_group, but very equivalent
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid: 1 - Fixed: nrounds:*280* - shrinkage:0.10 - max.tree:10
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Note: Just a trial. Grid should be extensive...
#Rest:  79.53%. 
#ExTm:  7.94mins.  

#---------
#--------- 
#---------
#Data: **17** variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 10, tuneGrid: 72 - Best: nrounds:300 - shrinkage:0.10 - max.tree:10
#Rest:  79.54%. 
#ExTm: 3.02 hours.  
#---------
#Data: 17 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 5, tuneGrid: 60 - Best: nrounds:300 - shrinkage:0.11 - max.tree:10
#Rest:  79.49%. 
#ExTm:  56 mins.  
#---------
#Data: 19 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 50, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:0.11 - max.tree:10
#Note: Errors in predictions due to "permit" and "public_..."
#Rest:  78.5%. 
#ExTm:  50 mins.  
#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 50, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:0.11 - max.tree:10
#Note: remove "permit" + "public...".
#Rest: 79.49 %. 
#ExTm:  30 mins.  
#---------
#Data: *17* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 100, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:0.11 - max.tree:10
#Rest:  79.49%. 
#ExTm:   57mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 10, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:0.11 - max.tree:10
#Note: Include "permit" + "public_..." but with preProcess = "knnImpute" or "bagImpute".
#Note: preProcess = "knnImpute". 
#Rest:  ~78.5%. 
#Rest: It is possible to predict, but the prediction has lower number of rows than original data, 
#so confusionMatrix is not possible to calculate...
#ExTm:   1.01 hours.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 10, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:0.11 - max.tree:10
#Note: Include "permit" + "public_..." but previously the data.frame was "rfImpute"
#Note: rfImpute not possible to apply for variables with more than 53 levels.
#Rest:  %. 
#ExTm:  hours.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: 50, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:0.11 - max.tree:10
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.96%. 
#ExTm:  33mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *100*, tuneGrid: 1 - Fixed: nrounds:300 - shrinkage:*0.105* - max.tree:10
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.09%. 
#ExTm:  55.97mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *50*, tuneGrid: 1 - Fixed: nrounds:*350* - shrinkage:0.10 - max.tree:10
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.17%. 
#ExTm:  33mins.  
#---------
#Data: *19* variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *50*, tuneGrid: 1 - Fixed: nrounds:*280* - shrinkage:0.10 - max.tree:10
#Note: Include "permit" + "public_..." but previously "na.omit"
#Rest:  79.18%. 
#ExTm:  29mins.  
#---------
#--------- 
#New extended dataset, with three new columns and no permit/public...
#water_quality has more levels than quality_group, but very equivalent
#Data: *20* variables - no: permit/public..- 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid: 1 - Fixed: nrounds:*280* - shrinkage:0.10 - max.tree:10
#Note: Included: region_code, extraction_code, water_quality. No: permit/public...
#Note: Just a trial. Grid should be extensive...
#Rest:  %. 
#ExTm:  mins.  
