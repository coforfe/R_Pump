
#----------------------------------------------------
#Working directory
setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp")


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
#datIncl <- datIncl[, -c(11,13)] #remove permit and  public_meeting.
datIncl$festatus <- str_replace_all(datIncl$festatus, " ", "_")
datIncl <- datIncl[, -c(3,5)] #remove funder, installer, if not it does not work... It cannot be all equal.

# Loading to use some new datIn columns (num_private)
library(data.table)
datTrain <- fread("training.csv")
datStat <- fread("trainingStatus.csv")
datIn <- datTrain[ , status_group:=datStat$status_group]
datIn <- as.data.frame(datIn)
rm(datTrain, datStat)

# Load also testing info to compare. with train.
datTestingori <- fread("testing.csv")
datTestingori <- as.data.frame(datTestingori)
datTesting <- fread("myTest.csv"); datTesting <- as.data.frame(datTesting)
#datTesting$num_private <- datTestingori$num_private

#To include all the remaining variables (6 more)
# source("changeTrainTest.R")
# tmpVal <- changeTrain()
# datIncl <- cbind.data.frame(tmpVal, datIncl)


#To transform in factors the character columns. It's very convenient.
for( i in 1:ncol(datIncl)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "character") {
    datIncl[, i] <- as.factor(datIncl[,i])
  } else next
}



#--------------------------------- 
#--------------------- Feature Engineering
#--------------------------------- 
detach("package:e1071", unload=TRUE)
library(imputeR)
# Kind of distance based on longitude/latitude
library(geosphere)
datIncl$fedist <- distGeo(as.matrix(datIncl[,c('longitude','latitude')]), c(0,0))
#datIncl$fedist3d <- sqrt(datIncl$fedist^2 + datIncl$gps_height^2)
datIncl$fepopdist <- c(datIncl$population / datIncl$fedist)
datIncl$fedistday <- c(datIncl$fedist / datIncl$date_recorded_offset_days )
datIncl$feamodist <- c(datIncl$amount_tsh / datIncl$fedist)
#datIncl$fedistCar <- sqrt(datIncl$longitude^2 + datIncl$latitude^2)
# Kind of utilization factor population/#pumps by region
DF <- as.data.table(datIncl)
poppumre <- DF[, .(.N, fesumpop= sum(population)), by="region"]
poppumre[, poppu:=fesumpop/N]
nona <- as.data.frame(poppumre)
nona$fesumpop <- ifelse(nona$fesumpop==0, NA, nona$fesumpop )
nona$poppu <- ifelse(nona$poppu==0, NA, nona$poppu )
nonatmp <- nona[,c(2,3,4)]
nonanona <- impute(as.matrix(nonatmp), lmFun="lassoR", verbose=FALSE)
nonaend <- as.data.frame(nonanona$imp)
names(nonaend) <- c('Nnew','fesumpopnew', 'poppunew')
nonadf <- cbind.data.frame(nona, nonaend)
nonaend <- nonadf[, c(1,2,7)]
names(nonaend) <- c('region', 'feN', 'fepoppun')
impTmp <- merge(datIncl, nonaend, by.x="region", by.y='region')
datIncl <- impTmp
tmpVar <- datIncl$festatus
datIncl <- datIncl[, -c(which(names(datIncl)=='festatus'))]
datIncl$festatus <- tmpVar

#Treat NAs in permit - public_meeting
perpub <- data.frame(permit=datIncl$permit, public_meeting=datIncl$public_meeting, festatus=datIncl$festatus)
newpermit <- ifelse(is.na(perpub$permit), NA, ifelse(perpub$permit=="TRUE",1, 0))
newpubmet <- ifelse(is.na(perpub$public_meeting), NA, ifelse(perpub$public_meeting=="TRUE",1, 0))
perpub$permit <- newpermit
perpub$public_meeting <- newpubmet
perpubnona <- as.data.frame(impute(perpub, cFun="rpartC", verbose=FALSE)$imp) 

datIncl$permit <- perpubnona$permit
datIncl$public_meeting <- perpubnona$public_meeting

#Clean workspace
rm(DF, impTmp, nona, nonadf,nonaend, nonatmp, perpub, perpubnona, poppumre, cltmp, i, newpermit, newpubmet,nonanona)

#To include new variables: num_private 
datIncl$num_private <- datIn$num_private
tmpVar <- datIncl$festatus
datIncl <- datIncl[, -c(which(names(datIncl)=='festatus'))]
datIncl$festatus <- tmpVar

#Transform factors in numeric 
for( i in 1:(ncol(datIncl)-1)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "factor") {
    datIncl[,i] <- as.numeric( datIncl[,i] )
  } else next
}

rm(tmpVar)


#--------------------------------- 
#---------------------- SEPARATE TRAINING - TESTING
#--------------------------------- 
library(caret)
set.seed(658754+rnorm(1)*10000)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]

#------------------ CHANGE ------------------
# Just non_functional and needs_repair
library(stringr)
#datSamp$festatus <- str_replace_all(datSamp$festatus, "functional_needs_repair",'non_functional')
datSamp <- datSamp[datSamp$festatus!="functional_needs_repair", ]
#datSamp$festatus <- ifelse(datSamp$festatus=="non_functional",0,1)
datSamp$festatus <- as.factor(as.vector(datSamp$festatus))

inTrain <- createDataPartition(datSamp$festatus, p=0.98)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]


#--------------------------------- 
#---------------------- RF - RANGER
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl(
  number=25,
  #method = "oob",
  #method = "boot632", repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
) 

#rfGrid <- expand.grid(.mtry=seq(5,7,1))
rfGrid <- expand.grid(.mtry=5)

modFitrf <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  tuneGrid = rfGrid,
  method = "ranger",
  num.trees = 500,
  importance = 'impurity',
  metric = "ROC",
  verbose = TRUE,
  classification = TRUE
)
modFitrf

#------------------- CHANGE
#Now what I get are probabilities...
predrf <- predict( modFitrf, newdata=testDat[,1:(ncol(testDat)-1)] , type="prob")

#ConfusionMatrix
#conMatrf <- confusionMatrix(testDat$festatus, predrf); conMatrf 
#conMatrfdf <- as.data.frame(conMatrf$overall); rfAcc <- conMatrfdf[1,1]; rfAcc <- as.character(round(rfAcc*100,2))
modFitrf$results
rfAcc <- modFitrf$results$ROC ; rfAcc
b <- Sys.time();b; b-a   

if( nrow(rfGrid) < 2  )  { resampleHist(modFitrf) } else  
{ plot(modFitrf, as.table=T) }

#Best iteration
modBest <- modFitrf$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitrf$times$final[3]
#Samples
samp <- dim(modFitrf$resample)[1] ; samp
numvars <- ncol(trainDat); numvars


#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitrf,
  file=paste("con_",numvars,"vars_rf_2class__n",samp,"_grid",modBestc,"_nobalan__",rfAcc,"__.RData", sep="")
)


#--------------------------------- 
#---------------------- FILE TO UPLOAD
#--------------------------------- 
library(caret)
library(data.table)
library(stringr)
datTestingori <- fread("testing.csv")
datTestingori <- as.data.frame(datTestingori)

datTesting <- fread("myTest.csv"); datTesting <- as.data.frame(datTesting)
#datTesting$num_private <- datTestingori$num_private

#--------------------------------- 
#--------------------- Feature Engineering
#--------------------------------- 

datTesting$num_private <- datTestingori$num_private

library(geosphere)
# datTesting$fedistGeo <- distGeo(as.matrix(datTesting[,c('longitude','latitude')]), c(0,0))
datTesting$fedist <- distGeo(as.matrix(datTesting[,c('longitude','latitude')]), c(0,0))
#datTesting$fedist3d <- sqrt(datTesting$fedist^2 + datTesting$gps_height^2)
#datTesting$fepopdist <- c(datTesting$population / datTesting$fedist)
#datTesting$fedistday <- c(datTesting$fedist / datTesting$date_recorded_offset_days )
#datTesting$feamodist <- c(datTesting$amount_tsh / datTesting$fedist)
#datTesting$fedistCar <- sqrt(datTesting$longitude^2 + datTesting$latitude^2)
# Kind of utilization factor population/#pumps by region
DF <- as.data.table(datTesting)
poppumre <- DF[, .(.N, fesumpop= sum(population)), by="region"]
poppumre[, poppu:=fesumpop/N]
nona <- as.data.frame(poppumre)
nona$fesumpop <- ifelse(nona$fesumpop==0, NA, nona$fesumpop )
nona$poppu <- ifelse(nona$poppu==0, NA, nona$poppu )
detach("package:e1071", unload=TRUE)
library(imputeR)
nonatmp <- nona[,c(2,3,4)]
nonanona <- impute(as.matrix(nonatmp), lmFun="lassoR", verbose=FALSE)
nonaend <- as.data.frame(nonanona$imp)
names(nonaend) <- c('Nnew','fesumpopnew', 'poppunew')
nonadf <- cbind.data.frame(nona, nonaend)
nonaend <- nonadf[, c(1,2,7)]
names(nonaend) <- c('region', 'feN', 'fepoppun')
impTmp <- merge(datTesting, nonaend, by.x="region", by.y='region')
datTesting <- impTmp


#Treat NAs in permit - public_meeting
perpub <- data.frame(permit=datTesting$permit, public_meeting=datTesting$public_meeting )
newpermit <- ifelse(is.na(perpub$permit), NA, ifelse(perpub$permit=="TRUE",1, 0))
newpubmet <- ifelse(is.na(perpub$public_meeting), NA, ifelse(perpub$public_meeting=="TRUE",1, 0))
perpub$permit <- newpermit
perpub$public_meeting <- newpubmet
#permit/public_management imputed  - gbmC it does not work...
library(imputeR)
perpubnona <- as.data.frame(impute(perpub, cFun="rpartC", verbose=FALSE)$imp) 

datTesting$permit <- perpubnona$permit
datTesting$public_meeting <- perpubnona$public_meeting

#To include all the remaining variables (6 more)
# source("changeTrainTest.R")
# valTest <- changeTest()
# datTesting <- cbind.data.frame(valTest, datTesting)



#To transform in factors the character columns. It's very convenient.
for( i in 1:ncol(datTesting)) {
  cltmp <- class(datTesting[, i])
  if(cltmp == "character") {
    datTesting[, i] <- as.factor(datTesting[,i])
  } else next
}

#Transform factors in numeric
for( i in 1:ncol(datTesting)) {
  cltmp <- class(datTesting[, i])
  if(cltmp == "factor") {
    datTesting[,i] <- as.numeric( datTesting[,i] )
  } else next
}

#Clean workspace
rm(DF, impTmp, nona, nonadf,nonaend, nonatmp, perpub, perpubnona, poppumre, cltmp, i, newpermit, newpubmet,nonanona)
rm(datTestingori)


library(stringr)
modFit <- modFitrf
#modFit <- modFitxgb
modtype <-modFit$method
samptmp <- modFit$resample; samp <- length(unique(samptmp$Resample))
numvars <- length(modFit$coefnames)
#scortmp <- word(nameData, 2, sep=fixed("__"))
scortmp <- rfAcc
#scortmp <- modFitxgb$results$ROC


#------------------- CHANGE
#Now what I get are probabilities...
#predConcurso_mod <- predict(modFit, newdata=datTesting, type='prob')
predConcurso_mod <- predict(modFit, newdata=datTesting)

#-------------------
#Play with probabilities to define each status
lim_nere <- 0.91
lim_nofu <- 0.90
predConcurso_mod$deci_nere <- ifelse(predConcurso_mod$functional_needs_repair > lim_nere, "functional_needs_repair",'pending')
predConcurso_mod$deci_nofu <- ifelse(predConcurso_mod$non_functional > lim_nofu, "non_functional",'pending')
predConcurso_mod$deci <- ifelse(
  predConcurso_mod[,3]==predConcurso_mod[,4],'functional',
  ifelse(predConcurso_mod[,3]=='functional_needs_repair','functional_needs_repair', 'non_functional')
)
prop.table(table(predConcurso_mod$deci))*100
predModify_mod <- str_replace_all(predConcurso_mod$deci, "_"," ")
#-------------------

predModify_mod <- str_replace_all(predConcurso_mod, "_"," ")
length(predConcurso_mod)
#File to updload
fileToUpload_mod <- data.frame(id=datTesting$id, status_group=predModify_mod)
head(fileToUpload_mod)

#To sort "ids" as datTesting initially...
datTmp <- fread("myTest.csv"); datTmp <- as.data.frame(datTmp)
datId <- data.frame(i=1:nrow(datTmp), id=datTmp$id)

datSubo <- merge(datId, fileToUpload_mod, by.xy='id', by.y='id', sort=FALSE )
datSuboend <- datSubo[,c(1,3)]; head(datSuboend)
#trainTab <- t(prop.table(table(trainDat$festatus))*100)
#testTab <- t(prop.table(table(testDat$festatus))*100)
datSubTab <- t(prop.table(table(datSuboend$status_group))*100)
tabEnd <- rbind(datSubTab, c(54.61, 7.19, 38.20))
rownames(tabEnd) <- c('datSub', 'Real')
tabEnd <- as.data.frame(tabEnd)
tabEnd[3,1] <- abs(tabEnd[1,1]-tabEnd[2,1])
tabEnd[3,2] <- abs(tabEnd[1,2]-tabEnd[2,2])
tabEnd[3,3] <- abs(tabEnd[1,3]-tabEnd[2,3])
rownames(tabEnd)[3] <- "diff"
sumEr <- round(sqrt(sum(tabEnd[3,]^2)),3) #sqrt(sum(var1^2 + var2^2...))
tabEnd <- rbind(tabEnd, c(sumEr,sumEr,sumEr))
rownames(tabEnd)[4] <- "sumDiff"
tabEnd

#scortmp <- 0.93
lim_nere <- 0
lim_nofu <- 0
timval <- str_replace_all(Sys.time(), " |:", "_")
write.table(datSuboend, file=paste("Res_xxxx_", modtype,"_Func-NonFunc_",numvars,"_nere_",lim_nere,"_nofu_",lim_nofu,"_samp",samp,"_Acc_", scortmp,"_Err_",sumEr,"_",timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

rm(datSubo, datSubTab, tabEnd, datId, datTmp)




#--------------------------------- 
#---------------------- XGB
#--------------------------------- 

a <- Sys.time();a
bootControl <- trainControl(
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
) 


# xgbGrid <- expand.grid(
#   .eta = 0.12,
#   .max_depth = 10,
#   .nrounds = 400
# )

  # xgbGrid <- expand.grid(
  #                        .eta = seq(0.11, 0.12, 0.01),
  #                        .max_depth = seq(10,11,1),
  #                        .nrounds = seq(300, 500,50)
  #                        )

# xgbGrid <- expand.grid(
#   .eta = seq(0.13, 0.14, 0.01),
#   .max_depth = seq(13,14, 1),
#   .nrounds = seq(500, 600,100 )
# )

# xgbGrid <- expand.grid(
#   .eta = 0.11,
#   .max_depth = 10,
#   .nrounds = 300
# )
  
C5grid <- expand.grid(model = "tree",
                      trials = 100,
                      winnow = FALSE)  


modFitxgb <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  metric = "ROC",
  tuneGrid = C5grid,
  method = "C5.0"
  # method = "xgbTree",
  #tuneGrid = xgbGrid,
  # verbose = 1,
  # num_class = 2
)
modFitxgb

predxgb <- predict( modFitxgb, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
#conMatxgb <- confusionMatrix(testDat$festatus, predxgb); conMatxgb 
#conMatxgbdf <- as.data.frame(conMatxgb$overall); xgbAcc <- conMatxgbdf[1,1]; xgbAcc <- as.character(round(xgbAcc*100,2))
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

