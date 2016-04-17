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

#------------------------------------
#Just "functional_needs_repair"... just before imputing...
#------------------------------------
datIncl <- datIncl[datIncl$festatus=="functional_needs_repair",]


#--------------------------------- 
#--------------------- Feature Engineering
#--------------------------------- 

library(imputeR)
# Kind of distance based on longitude/latitude
library(geosphere)
datIncl$fedist <- distGeo(as.matrix(datIncl[,c('longitude','latitude')]), c(0,0))
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
datIncl$num_private <- datIn$num_private[datIn$status_group=="functional needs repair"]
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
set.seed(658754)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]


#--------------------------------- 
#---------------------- RF
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(457856) 
bootControl <- trainControl(method='oob',number=25, verboseIter=TRUE) 
#bootControl <- trainControl(number=5, verboseIter=TRUE) 

#rfGrid <- expand.grid(.mtry=seq(2,20,1))
rfGrid <- expand.grid(.mtry=5)

modFitrf <-  train(
  festatus ~ .,
  data = trainDat,
  method = "rf",
  trControl = bootControl,
  tuneGrid = rfGrid,
  do.trace = TRUE,
  ntree =  500
)

predrf <- predict( modFitrf, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatrf <- confusionMatrix(testDat$festatus, predrf); conMatrf 
conMatrfdf <- as.data.frame(conMatrf$overall); rfAcc <- conMatrfdf[1,1]; rfAcc <- as.character(round(rfAcc*100,2))
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

#Variable Importance
Imprf <- varImp( modFitrf, scale=F)
plot(Imprf, top=(ncol(trainDat)-1))

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitrf,
  file=paste("con_",numvars,"vars_rf_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",rfAcc,"__.RData", sep="")
)



comenta <- function() {
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "boot632" + ntrees=500 + seed() nueva. 
  # Summ: It worsens. Error: 8.43% (new way calculated)
  # Rest: 81.10%  - Platform: 
  # ExTm: 58.17% mins.
  # Accuracy : 0.811           
  # 95% CI : (0.8052, 0.8168)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 2:20 -> mtry= - no "oob" + ntrees=500 + seed() nueva. 
  # Summ: Erro 13.15%. A little bit lower...
  # Rest:  81.03$ - Platform: 
  # ExTm: 4.94 hours
  # Accuracy : 0.8103         
  # 95% CI : (0.8044, 0.816)
  
  #----- 2015-10-09 
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 2:20 -> mtry=6  - "oob" + ntrees=500 + seed() nueva. 
  # Summ: Improves a little. Error: 12.53%
  # Rest:  81.15% - Platform: 81.17%
  # ExTm:  35.69mins
  # Accuracy : 0.8115          
  # 95% CI : (0.8057, 0.8172)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=6. - "oob" + ntrees=500 + seed() nueva. 
  # Summ: With 25 and new seed...worst - Error: 12.77%. (error: 8.07%)
  # Rest:  81.12% - Platform: 81.30% (**The best!**) 
  # ExTm:  5.35mins
  # Accuracy : 0.8112          
  # 95% CI : (0.8053, 0.8169)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=6. - no "oob" + ntrees=500 + seed() nueva. 
  # Summ: With 25 and new seed...worst - Error: 12.16%.
  # Rest:  81.15% - Platform: 81.22%
  # ExTm:  1.05hours
  # Accuracy : 0.8115          
  # 95% CI : (0.8057, 0.8173)
  
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 1 -> mtry=6. - "oob" + ntrees=1000. - New distance fedistCar
  # Summ: Lower result and 13% error in Upload.Same result but higher error with data upload. 13%. 
  # Rest:  81.21% - Platform: (not uploaded)
  # ExTm:  6.65mins
  # Accuracy : 0.8123         
  # 95% CI : (0.8065, 0.818)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 1 -> mtry=6. - "oob" + ntrees=1000.
  # Summ: Same result but higher error with data upload. 13%. 
  # Rest:  81.35% - Platform: 81.08%.
  # ExTm:  12.95mins
  # Accuracy : 0.8135          
  # 95% CI : (0.8077, 0.8192)  
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 4:10 - best mtry=6. - "oob" + ntrees=500.
  # Summ: Better with oob y more trees.  - 12% error.
  # Rest:  81.35% - Platform: 81.14%.
  # ExTm:  18.52mins
  # Accuracy : 0.8135          
  # 95% CI : (0.8077, 0.8192)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 4:10 - best mtry=6.
  # Summ: (lower than time ago... got 81.38% ??).  - 12% error.
  # Rest: 81.13% - Platform: 81.07%.
  # ExTm:  18.52mins
  # Accuracy : 0.8113         
  # 95% CI : (0.8054, 0.817)
  
  
}


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
#datTesting$fedistCar <- sqrt(datTesting$longitude^2 + datTesting$latitude^2)
# Kind of utilization factor population/#pumps by region
DF <- as.data.table(datTesting)
poppumre <- DF[, .(.N, fesumpop= sum(population)), by="region"]
poppumre[, poppu:=fesumpop/N]
nona <- as.data.frame(poppumre)
nona$fesumpop <- ifelse(nona$fesumpop==0, NA, nona$fesumpop )
nona$poppu <- ifelse(nona$poppu==0, NA, nona$poppu )
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
modtype <-modFit$method
samptmp <- modFit$resample; samp <- length(unique(samptmp$Resample))
numvars <- length(modFit$coefnames)
#scortmp <- word(nameData, 2, sep=fixed("__"))
scortmp <- rfAcc
#scortmp <- xgbAcc

predConcurso_mod <- as.vector(predict(modFit, newdata=datTesting))
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
trainTab <- t(prop.table(table(trainDat$festatus))*100)
testTab <- t(prop.table(table(testDat$festatus))*100)
datSubTab <- t(prop.table(table(datSuboend$status_group))*100)
tabEnd <- rbind(trainTab,testTab, datSubTab, c(54.61, 7.19, 38.20))
rownames(tabEnd) <- c('train','test','datSub', 'Real')
tabEnd <- as.data.frame(tabEnd)
tabEnd[5,1] <- abs(tabEnd[3,1]-tabEnd[4,1])
tabEnd[5,2] <- abs(tabEnd[3,2]-tabEnd[4,2])
tabEnd[5,3] <- abs(tabEnd[3,3]-tabEnd[4,3])
rownames(tabEnd)[5] <- "diff"
sumEr <- round(sqrt(sum(tabEnd[5,]^2)),3) #sqrt(sum(var1^2 + var2^2...))
tabEnd <- rbind(tabEnd, c(sumEr,sumEr,sumEr))
rownames(tabEnd)[6] <- "sumDiff"
tabEnd


timval <- str_replace_all(Sys.time(), " |:", "_")
write.table(datSuboend, file=paste("Res_xxxx_", modtype,"_",numvars,"_samp100_noban_n",samp,"_Acc_", scortmp,"_Err_",sumEr,"_",timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

rm(datSubo, trainTab, testTab, datSubTab, tabEnd, datId, datTmp)
