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

#------- downSampling
datDown <- downSample(x=datIncl[,1:16], y=datIncl[,17], yname="festatus")

library(caret)
set.seed(1)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datDown)
datSamp <- datDown[sample(1:nrow(datDown), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]


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

bootControl <- trainControl(
                            repeats = 5,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            verboseIter = TRUE
                            ) 


# xgbGrid <- expand.grid(
#   .eta = 0.12,
#   .max_depth = 10,
#   .nrounds = 400
# )

#   xgbGrid <- expand.grid(
#                          .eta = seq(0.11, 0.12, 0.01),
#                          .max_depth = seq(1,4,1),
#                          .nrounds = seq(100, 300,50)
#                          )

# xgbGrid <- expand.grid(
#   .eta = seq(0.13, 0.14, 0.01),
#   .max_depth = seq(13,14, 1),
#   .nrounds = seq(500, 600,100 )
# )

xgbGrid <- expand.grid(
  .eta = 0.11,
  .max_depth = 10,
  .nrounds = 300
)

# xgbGrid <- expand.grid(
#   .eta = 0.10, 
#   .max_depth = seq(10,15, 1),
#   .nrounds = 200
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
  #file=paste("xgb_17var_3class_samp100_n",samp,"_grid",modBestc,"_Downbalan__",xgbAcc,"__.RData", sep="")
  file=paste("concurso_33vars_xgb_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",xgbAcc,"__.RData", sep="")
)


#-----------------------------------------------------------
#-----------------------------------------------------------
# TESTING - FILES TO UPLOAD
#-----------------------------------------------------------
#-----------------------------------------------------------
library(caret)
library(data.table)
library(stringr)
# > dim(datTesting)
# [1] 14850    41
datTesting <- fread("testing.csv")
datTesting <- as.data.frame(datTesting)

fehowold <- ifelse(datTesting$construction_year==0,0, 2015-datTesting$construction_year)
datTesting$fehowold <- fehowold
datTesting <- as.data.frame(datTesting)

for( i in 1:ncol(datTesting)) {
  cltmp <- class(datTesting[, i])
  if(cltmp == "character") {
    datTesting[, i] <- as.factor(datTesting[,i])
  } else next
}

load("concurso_32vars_rf_3class_samp100_n5_grid23_nobalan__80.86__.RData")
datTesting <- fread("myTest.csv"); datTesting <- as.data.frame(datTesting)
predConcurso_xgb <- as.vector(predict(modFitxgb, newdata=datTesting))
predModify_xgb <- str_replace_all(predConcurso_xgb, "_"," ")
length(predConcurso_xgb)
#File to updload
fileToUpload_xgb <- data.frame(id=datTesting$id, status_group=predModify_xgb)
head(fileToUpload_xgb)
write.table(fileToUpload_xgb, file="fileToUpload_xgb3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


datTesting$fedist <- sqrt(datTesting$longitude^2 + datTesting$latitude^2)
# Kind of utilization factor population/#pumps by region
DF <- as.data.table(datTesting)
poppumre <- DF[, .(.N, fesumpop= sum(population)), by="region"]
poppumre[, poppu:=fesumpop/N]
nona <- as.data.frame(poppumre)
nona$fesumpop <- ifelse(nona$fesumpop==0, NA, nona$fesumpop )
nona$poppu <- ifelse(nona$poppu==0, NA, nona$poppu )
library(imputeR)
nonatmp <- nona[,c(2,3,4)]
nonanona <- impute(as.matrix(nonatmp), lmFun="lassoR")
nonaend <- as.data.frame(nonanona$imp)
names(nonaend) <- c('Nnew','fesumpopnew', 'poppunew')
nonadf <- cbind.data.frame(nona, nonaend)
nonaend <- nonadf[, c(1,2,7)]
names(nonaend) <- c('region', 'feN', 'fepoppun')
impTmp <- merge(datTesting, nonaend, by.x="region", by.y='region')
datTesting <- impTmp

#To sort "ids" as datTesting initially...
datTmp <- fread("myTest.csv"); datTmp <- as.data.frame(datTmp)
datId <- data.frame(i=1:nrow(datTmp), id=datTmp$id)

datSubo <- merge(datId, fileToUpload_xgb, by.xy='id', by.y='id', sort=FALSE )
datSuboend <- datSubo[,c(1,3)]; head(datSuboend)
trainTab <- t(prop.table(table(trainDat$festatus))*100)
testTab <- t(prop.table(table(testDat$festatus))*100)
datSubTab <- t(prop.table(table(datSuboend$status_group))*100)
tabEnd <- rbind(trainTab,testTab, datSubTab)
rownames(tabEnd) <- c('train','test','datSub')
tabEnd

write.table(datSuboend, file="fileToUpload_xgb3.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)
