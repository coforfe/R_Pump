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
#http://topepo.github.io/caret/sampling.html
#http://topepo.github.io/caret/preprocess.html#impute
#http://stackoverflow.com/questions/20054906/carettrain-values-not-imputed
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
#Remove columns with more values in the "training.csv" file.
#And others...repeated.
library(stringr)
#Inconsistent:  'funder','installer','wpt_name', 'subvillage','ward','scheme_name',
toRem <- c(
  'id',
  'funder','installer','wpt_name', 'subvillage','ward','scheme_name',
  'longitude', 'latitude','date_recorded', 'water_quality', 'region_code',
  'district_code','recorded_by','extraction_type_group', 'extraction_type_class',
  'management_group','payment_type','quantity_group','source_type',
  'source_class', 'waterpoint_type_group')
datIncl <- datIn[, !(names(datIn) %in% toRem) ]

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



#--------------------------------------------
#-------------- upSampling
#--------------------------------------------
library(caret)
# Just to see if it is possible to model, a 25%  sample of the whole set.
datUp <- upSample(datIncl[,1:16], datIncl[,17], yname="festatus") #for toOut - no installer
set.seed(1)
#sizModup <- 0.25 * nrow(datUp)
sizModup <- 1 * nrow(datUp)
datSampUp <- datUp[sample(1:nrow(datUp), sizModup) , ]

inTrainup <- createDataPartition(datSampUp$festatus, p=0.70)[[1]]
trainDat <- datSampUp[ inTrainup, ]
testDat <- datSampUp[ -inTrainup, ]


#----------------------------------------------------
# MODELS
#----------------------------------------------------

#--------------------------------- 
#---------------------- XGB
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 


xgbGrid <- expand.grid(
  .eta = seq(0.10, 0.20, 0.01),
  .max_depth = seq(10,20, 1),
  .nrounds = seq(400, 1000,100 )
)


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

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitxgb,
  file=paste("xgb_17var_3class_samp100_n",samp,"_grid",modBestc,"_Upbalan__",xgbAcc,"__.RData", sep="")
)


