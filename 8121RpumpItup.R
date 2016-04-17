
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
#datIncl$fepopdist <- c(datIncl$population / datIncl$fedist)
#datIncl$fedistday <- c(datIncl$fedist / datIncl$date_recorded_offset_days )
#datIncl$feamodist <- c(datIncl$amount_tsh / datIncl$fedist)
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
nonanona <- nonatmp
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
detach("package:imputeR", unload=TRUE)


#--------------------------------- 
#---------------------- SEPARATE TRAINING - TESTING
#--------------------------------- 
library(caret)
set.seed(658754+rnorm(1)*10000)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.95)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]

#load("con_35vars_rf_3class_samp100_n_grid5_nobalan__82.97__.RData")
#--------------------------------- 
#---------------------- RF - RANGER
#--------------------------------- 
library(doMC)
numCor <- parallel::detectCores() - 2
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
set.seed(187)
#bootControl <- trainControl(method='boot',number=50, verboseIter=TRUE) 
#bootControl <- trainControl(method='oob',number=25, verboseIter=TRUE) 
bootControl <- trainControl(number=15, verboseIter=TRUE) 

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
  respect.unordered.factors = TRUE,
  verbose = TRUE,
  classification = TRUE
  #method = "rf",
  #do.trace = TRUE,
  #ntree =  500
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
  
  # --- NEW Ranger-Version
  # --- Remove all "fe*" new variables except fedist with a lot of improvement.
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 38 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *50*, tuneGrid 1 -> mtry=5 - (no "oob") ntrees=500 + seed() nueva. 
  # Expe: Change *99%* train - 1% test.
  # Summ: 82.97% - Error: 8.58%
  # Rest: 82.97%  - Platform: %
  # ExTm: 31.46mins
  # Accuracy : 0.8297         
  # 95% CI : (0.797, 0.8591)
  
  #2015-11-12 - 0.8198   
  
  # --- NEW Ranger-Version
  # --- Remove all "fe*" new variables except fedist with a lot of improvement.
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 38 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - (no "oob") ntrees=500 + seed() nueva. 
  # Expe: Change *99%* train - 1% test.
  # Summ: 83.14% - Error: 8.72%
  # Rest: 83.14%  - Platform: %
  # ExTm: 31.46mins
  # Accuracy : 0.8314          
  # 95% CI : (0.7988, 0.8606)
  
  #2015-11-12 - 0.8198   
  
  # --- Remove all "fe*" new variables except fedist with a lot of improvement.
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 38 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change *99%* train - 1% test.
  # Summ: 83.64% - Error: 8.80% 
  # Rest: 83.64%  - Platform: 81.58%
  # ExTm: 2.29min 
  # Accuracy : 0.8364          
  # 95% CI : (0.8042, 0.8653)
  
  #2015-10-25 - 0.8198  
  
  # --> Include a new variable "feamodist -> amount_tsh / fedist"
  # --- Include a new variable "fepopdist -> population / fedist"
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 38 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *50*, tuneGrid 1 -> mtry=6 - "oob" + ntrees=*700* + seed() nueva. 
  # Expe: Change *90%* train - 10% test.
  # Summ: 81.02% - It worsens...hmmm!
  # Rest: 81.02%  - Platform: 
  # ExTm: 4.19min 
  # Accuracy : 0.8102       
  # 95% CI : (0.8, 0.8201)
  
  # --- Include a new variable "fepopdist -> population / fedist"
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=6 - "oob" + ntrees=*500* + seed() nueva. 
  # Expe: Change *90%* train - 10% test.
  # Summ:81.39% - It worsens with lower bootstraps and lower trees??. 
  # Rest: 81.39%  - Platform: 
  # ExTm: 3.76min 
  # Accuracy : 0.8139          
  # 95% CI : (0.8038, 0.8237)
  
  
  # --- Include a new variable "fepopdist -> population / fedist"
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 50, tuneGrid 1 -> mtry=6 - "oob" + ntrees=700 + seed() nueva. 
  # Expe: Change *99%* train - 1% test.
  # Summ: 81.28%... It does not improve a very much (99%)
  # Rest: 81.28%  - Platform: 
  # ExTm: 6.02min 
  # Accuracy : 0.8128         
  # 95% CI : (0.779, 0.8434)
  
  
  # --- Include a new variable "fepopdist -> population / fedist"
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 50, tuneGrid 1 -> mtry=6 - "oob" + ntrees=700 + seed() nueva. 
  # Expe: Change *95%* train - 5% test.
  # Summ: 81.13%... It worsens with 95%...
  # Rest: %  - Platform: 
  # ExTm: 4.85min 
  # Accuracy : 0.8113          
  # 95% CI : (0.7968, 0.8253)
  
  # --- Include a new variable "fepodist -> population / fedist"
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 50, tuneGrid 1 -> mtry=6 - "oob" + ntrees=700 + seed() nueva. 
  # Expe: Change *90%* train - 10% test.
  # Summ: 81.91% - It improves a lot!. with 90% population...
  # Rest: 81.91%  - Platform: 
  # ExTm: 4.79min 
  # Accuracy : 0.8191          
  # 95% CI : (0.8091, 0.8288)
  
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 25, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99.5% train - 1.5% test.
  # Summ: 81.42% - It does not improve, but CI is higher.
  # Rest: 81.42%  - Platform: 
  # ExTm: 3.59min 
  # Accuracy : 0.8142          
  # 95% CI : (0.7651, 0.8568)
  
  #2015-10-19 - 0.8198  
  
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 25, tuneGrid 1 -> mtry=*6* - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change *99%* train - 1% test.
  # Summ: It improves mtry=6.
  # Rest:  81.79% - Platform: 0.8172 
  # ExTm: 2.58min 
  # Accuracy : 0.8179          
  # 95% CI : (0.7844, 0.8481)
  
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 25, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change *99%* train - 1% test.
  # Summ: It worsens... 81.28%
  # Rest:  81.28% - Platform: 0.8153
  # ExTm: 2.34min 
  # Accuracy : 0.8128         
  # 95% CI : (0.779, 0.8434)
  
  # --- Include a new variable fedist3d -> sqrt(fedist^2 + gps_height^2)
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: 25, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 98.5% train - 1.5% test.
  # Summ: 81.55% - Not a lot of improvement...
  # Rest: 81.55%  - Platform: 0.8172 
  # ExTm: 2.41min 
  # Accuracy : 0.8155          
  # 95% CI : (0.7884, 0.8405)
  
  #2015-10-19 - 0.8198 
  
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 2:20 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99% train - 1% test.
  # Summ: It took a lot of time... 13 hours... 
  # Rest: 81.11%  - Platform: 
  # ExTm: 13.30hours 
  # Accuracy : 0.8111          
  # 95% CI : (0.7773, 0.8419)
  
  #2015-10-13 - 0.8198
  
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 5:7 -> mtry=6 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99% train - 1% test.
  # Summ: It improves significatively. And the CI !!!  - Estimated error: 8.06%.
  # Rest: 81.96%  - Platform: 
  # ExTm: 6.94mins 
  # Accuracy : 0.8196          
  # 95% CI : (0.7862, 0.8497)
  
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 4:6 -> mtry=6 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99% train - 1% test.
  # Summ: It worsens significatively.
  # Rest: 81.11%  - Platform: 
  # ExTm: 6.04mins (speed up 5x with respect to RF!!!!)
  # Accuracy : 0.8111          
  # 95% CI : (0.7773, 0.8419)
  
  #2015-10-13 - 0.8198
  
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99% train - 1% test.
  # Summ: It improves significatively. And CI!!.
  # Rest: 82.97%  - Platform: 
  # ExTm: 2.02mins 
  # Accuracy : 0.8297         
  # 95% CI : (0.797, 0.8591)
  
  
  # --- WITH RANGER
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 98% train - 2% test.
  # Summ: It worsens a little bit.
  # Rest: 82.39%  - Platform: 
  # ExTm: 1.99mins (speed up 5x with respect to RF!!!!)
  # Accuracy : 0.8239         
  # 95% CI : (0.801, 0.8452)
  
  #2015-10-11 - Repeat winner scenario 0.8212.  
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - NO "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 98% train - 2% test. Error: 8.06%.
  # Summ: Improves. 83.15 (The best!!) 
  # Rest: 83.15%  - Platform: 
  # ExTm: 8.02mins
  # Accuracy : 0.8315         
  # 95% CI : (0.809, 0.8524) -> the best CI!.
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - NO "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99.5% train - 0.5% test.
  # Summ: It worsens a lot 0.7534. 
  # Rest: 75.34%  - Platform: 
  # ExTm: 9.84mins
  # Accuracy : 0.7534          
  # 95% CI : (0.7002, 0.8014)
  
  #2015-10-11 - Repeat winner scenario 0.8212. 
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - NO "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99% train - 1% test.
  # Summ: It worsens. Error: 7.66% !! (new way calculated)
  # Rest: 80.44%  - Platform: 
  # ExTm: 1.28hours
  # Accuracy : 0.8044          
  # 95% CI : (0.7701, 0.8356)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 99% train - 1% test.
  # Summ: The best result!!. Error: 7.66% !! (new way calculated)
  # Rest: 82.12%  - Platform: 
  # ExTm: 8.10mins
  # Accuracy : 0.8212         
  # 95% CI : (0.788, 0.8513)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 95% train - 5% test.
  # Summ: It worsen, but increase CI!!!. Error: 7.44% !! (new way calculated)
  # Rest: 81.06%  - Platform: 
  # ExTm: 7.69mins
  # Accuracy : 0.8106          
  # 95% CI : (0.7961, 0.8246)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 90% train - 10% test.
  # Summ: It worsen a little!. Error: 8.59% (new way calculated)
  # Rest: 81.37%  - Platform: 
  # ExTm: 7.24mins
  # Accuracy : 0.8137          
  # 95% CI : (0.8036, 0.8236)
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *25*, tuneGrid 1 -> mtry=5 - "oob" + ntrees=500 + seed() nueva. 
  # Expe: Change 80% train - 20% test.
  # Summ: It improves a lot!. Error: 8.52% (new way calculated)
  # Rest: 81.59%  - Platform: 
  # ExTm: 5.57mins
  # Accuracy : 0.8159          
  # 95% CI : (0.8088, 0.8228)
  
  
  # --- Back to best scenario 81.21%.
  # --- New variables : num_private, dist + numbpump region + people/pump  + public/permit
  # Data: 34 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *40*, tuneGrid 1 -> mtry=6 - "oob" + ntrees=500 + seed() nueva. 
  # Summ: It worsens. Error: 8.04% (new way calculated)
  # Rest: 80.81%  - Platform: 
  # ExTm: 5.57mins
  # Accuracy : 0.8081          
  # 95% CI : (0.8022, 0.8138)
  
  #2015-10-11 - Repeat winner scenario 0.8130. 
  
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
#modFit <- modFitrfer
#modFit <- modFitxgb
#modFit <- modFitgbm
#modFit <- modFitada
#modFit <- modFitbor
modtype <-modFit$method
samptmp <- modFit$resample; samp <- length(unique(samptmp$Resample))
numvars <- length(modFit$coefnames)
#scortmp <- word(nameData, 2, sep=fixed("__"))
scortmp <- rfAcc
#scortmp <- rferAcc
#scortmp <- adaAcc
#scortmp <- xgbAcc
#scortmp <- gbmAcc
#scortmp <- borAcc

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





#--------------------------------- 
#---------------------- XGB - C5.0
#--------------------------------- 

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
# bootControl <- trainControl(
#   number = 5,
#   # summaryFunction = twoClassSummary,
#   # classProbs = TRUE,
#   verboseIter = TRUE
# ) 
bootControl <- trainControl(method="cv",number=5, verboseIter=TRUE) 


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
#   .eta = seq(0.12, 0.13, 0.01),
#   .max_depth = seq(12,13, 1),
#   .nrounds = 300
# )

# xgbGrid <- expand.grid(
#   .eta = 0.12,
#   .max_depth = 13,
#   .nrounds = 1600
# )

#http://stackoverflow.com/questions/26140218/using-cost-sensitive-c50-in-caret
# cost.matrix <- matrix(c(
#   1, 3, 1,
#   3, 1, 2,
#   1, 2, 1
# ), 3, 3, byrow=TRUE)
# rownames(cost.matrix) <- colnames(cost.matrix) <- c("functional", "functional_needs_repair", "non_functional")

C5grid <- expand.grid( model = "tree",
                       trials = 1:100,
                       #trials = c((1:15)*10, rep(80,10)),
                       winnow = FALSE
                       #cost = 1:10
                     )


modFitxgb <-  train(
  festatus ~ .,
   # x = trainDat[, 1:(ncol(trainDat)-1)],
   # y = trainDat[, ncol(trainDat)], 
  data = trainDat,
  trControl = bootControl,
  metric = "Accuracy",
  tuneGrid = C5grid,
  method = "C5.0"
  #method = "C5.0Cost"
  # metric = "ROC",
  # method = "xgbTree",
  # tuneGrid = xgbGrid,
  # verbose = 1,
  # num_class = 3
)

modFitxgb

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


#--------------------------------- 
#---------------------- GBM
#--------------------------------- 
library(doMC)
numCor <- parallel::detectCores() - 2
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

# xgbGrid <- expand.grid(
#   .eta = seq(0.12, 0.13, 0.01),
#   .max_depth = seq(12,13, 1),
#   .nrounds = 300
# )

# gbmGrid <- expand.grid(
#   interaction.depth = seq(11,15,2),
#   n.trees = seq(800,1000,50),
#   shrinkage = seq(0.08,0.1,0.01),
#   n.minobsinnode = seq(17,20,1)
# )

gbmGrid <- expand.grid(
  interaction.depth = 15,
  n.trees = 1000,
  shrinkage = 0.1,
  n.minobsinnode = 20
)

modFitgbm <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  metric = "Accuracy",
  method = "gbm",
  tuneGrid = gbmGrid,
  verbose = TRUE
)
modFitgbm

predgbm <- predict( modFitgbm, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatgbm <- confusionMatrix(testDat$festatus, predgbm); conMatgbm 
conMatgbmdf <- as.data.frame(conMatgbm$overall); gbmAcc <- conMatgbmdf[1,1]; gbmAcc <- as.character(round(gbmAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(gbmGrid) < 2  )  { resampleHist(modFitgbm) } else  
{
  plot(modFitgbm, as.table=T) 
  ggplot(modFitgbm) + theme(legend.position="top")
 }

# #Variable Importance
# Impxgb <- varImp( modFitxgb, scale=F)
# plot(Impxgb, top=20)

#Best iteration
modBest <- modFitgbm$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitgbm$times$final[3]
#Samples
samp <- dim(modFitgbm$resample)[1]

comentagbm <- function() {
  
  
# gbmGrid <- expand.grid(
#   interaction.depth = 11,
#   n.trees = 800,
#   shrinkage = 0.08,
#   n.minobsinnode = 17
# )
  # Accuracy : 0.7943          
  # 95% CI : (0.7883, 0.8002)  
  
  
# gbmGrid <- expand.grid(
#   interaction.depth = 9,
#   n.trees = 700,
#   shrinkage = 0.05,
#   n.minobsinnode = 15
# )  
  # Accuracy : 0.7913          
  # 95% CI : (0.7852, 0.7972) 
  
# gbmGrid <- expand.grid(
#   interaction.depth = 7,
#   n.trees = 500,
#   shrinkage = 0.05,
#   n.minobsinnode = 10
# )
  # Accuracy : 0.7837          
  # 95% CI : (0.7775, 0.7897)

# > gbmGrid <- expand.grid(
#   +   interaction.depth = 5,
#   +   n.trees = 500,
#   +   shrinkage = 0.05,
#   +   n.minobsinnode = 5
#   + )
# Accuracy : 0.7781          
# 95% CI : (0.7719, 0.7842)
  
}  



#--------------------------------- 
#---------------------- ADA
#--------------------------------- 

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl(method="cv",number=5, verboseIter=TRUE) 

Adagrid <- expand.grid( 
                       mfinal = 150 , 
                       maxdepth = 30
                       #coeflearn = 'Breiman'
                      )


modFitada <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  metric = "Accuracy",
  tuneGrid = Adagrid,
  method = "AdaBag"
  #method = "AdaBoost.M1"
)

modFitada

predada <- predict( modFitada, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatada <- confusionMatrix(testDat$festatus, predada); conMatada 
conMatadadf <- as.data.frame(conMatada$overall); adaAcc <- conMatadadf[1,1]; adaAcc <- as.character(round(adaAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(AdaGrid) < 2  )  { resampleHist(modFitada) } else  
{ plot(modFitada, as.table=T) }

# #Variable Importance
# Impxgb <- varImp( modFitxgb, scale=F)
# plot(Impxgb, top=20)

#Best iteration
modBest <- modFitada$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitada$times$final[3]
#Samples
samp <- dim(modFitada$resample)[1]


#--------------------------------- 
#---------------------- KOHONEN
#--------------------------------- 

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl(number=25, verboseIter=TRUE) 

xyfGrid <- expand.grid( 
  xdim = seq(6,6,5), 
  ydim = seq(1,6,5),
  xweight = seq(1,6,5),
  topo = 'hexagonal' 
)


modFitxyf <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  metric = "Accuracy",
  tuneGrid = xyfGrid,
  method = "xyf"
)

modFitxyf

predxyf <- predict( modFitxyf, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatxyf <- confusionMatrix(testDat$festatus, predxyf); conMatxyf 
conMatxyfdf <- as.data.frame(conMatxyf$overall); xyfAcc <- conMatxyfdf[1,1]; xyfAcc <- as.character(round(xyfAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(xyfGrid) < 2  )  { resampleHist(modFitxyf) } else  
{ plot(modFitxyf, as.table=T) }

# #Variable Importance
# Impxgb <- varImp( modFitxgb, scale=F)
# plot(Impxgb, top=20)

#Best iteration
modBest <- modFitxyf$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitxyf$times$final[3]
#Samples
samp <- dim(modFitxyf$resample)[1]


#--------------------------------- 
#---------------------- BORUTA
#--------------------------------- 

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

borGrid <- expand.grid(mtry=5)

modFitbor <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  metric = "Accuracy",
  tuneGrid = borGrid,
  method = "Boruta"
)
modFitbor

predbor <- predict( modFitbor, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatbor <- confusionMatrix(testDat$festatus, predbor); conMatbor 
conMatbordf <- as.data.frame(conMatbor$overall); borAcc <- conMatbordf[1,1]; borAcc <- as.character(round(borAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(borGrid) < 2  )  { resampleHist(modFitbor) } else  
{ plot(modFitbor, as.table=T) }

#Variable Importance
Impbor <- varImp( modFitbor, scale=F)
plot(Impbor, top=20)

#Best iteration
modBest <- modFitbor$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitbor$times$final[3]
#Samples
samp <- dim(modFitbor$resample)[1]

# Accuracy : 0.8045          
# 95% CI : (0.7808, 0.8268)
# Time difference of 6.943102 hours


#--------------------------------- 
#---------------------- RFERNS
#--------------------------------- 
# Random Ferns (method = 'rFerns')
# For classification using package rFerns with tuning parameters:
# Fern Depth (depth, numeric)


a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl( number=5, verboseIter=TRUE) 
#method = "oob" not implemented for this model.

#rferGrid <- expand.grid( .depth = seq(9,16,1)) 
rferGrid <- expand.grid( .depth = 15) 
                         #.numeric = 1000 )#error when including this 

modFitrfer <-  train(
  #festatus ~ .,
  #data = trainDat,
  x = trainDat[, 1:(ncol(trainDat)-1)],
  y = trainDat[, ncol(trainDat)],
  trControl = bootControl,
  metric = "Accuracy",
  tuneGrid = rferGrid,
  method = "rFerns",
  ferns = 5000
)
modFitrfer

predrfer <- predict( modFitrfer, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatrfer <- confusionMatrix(testDat$festatus, predrfer); conMatrfer 
conMatrferdf <- as.data.frame(conMatrfer$overall); rferAcc <- conMatrferdf[1,1]; rferAcc <- as.character(round(rferAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(rferGrid) < 2  )  { resampleHist(modFitrfer) } else  
{ plot(modFitrfer, as.table=T) }

#Variable Importance
# Imprfer <- varImp( modFitrfer, scale=F)
# plot(Imprfer, top=20)

#Best iteration
modBest <- modFitrfer$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitrfer$times$final[3]
#Samples
samp <- dim(modFitrfer$resample)[1]

# rferGrid <- expand.grid( .depth = 15) 
# ferns = 5000
# Accuracy : 0.6099          
# 95% CI : (0.5815, 0.6378)
# This model gets 35% of functional_needs_repair and very bad "functional":27%.

# rferGrid <- expand.grid( .depth = seq(9,16,1)) 
# Accuracy : 0.6125          
# 95% CI : (0.5841, 0.6403)
# Time difference of 2.811328 mins


#--------------------------------- 
#---------------------- GAM
#--------------------------------- 
# Generalized Additive Model using Splines (method = 'gam')
# For classification and regression using package mgcv with tuning parameters:
# Feature Selection (select, logical)
# Method (method, character)


a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl( number=5, verboseIter=TRUE) 

#gamGrid <- expand.grid( .depth = seq(9,16,1)) 
gamGrid <- expand.grid( select = TRUE , method = "GCV.Cp") 

modFitgam <-  train(
  festatus ~ .,
  data = trainDat,
  # x = trainDat[, 1:(ncol(trainDat) - 1)],
  # y = trainDat[, ncol(trainDat)],
  trControl = bootControl,
  metric = "Accuracy",
  tuneGrid = gamGrid,
  method = "gam"
)
modFitgam

predgam <- predict( modFitgam, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatgam <- confusionMatrix(testDat$festatus, predgam); conMatgam 
conMatgamdf <- as.data.frame(conMatgam$overall); gamAcc <- conMatgamdf[1,1]; gamAcc <- as.character(round(gamAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(gamGrid) < 2  )  { resampleHist(modFitgam) } else  
{ plot(modFitgam, as.table=T) }

#Variable Importance
# Impgam <- varImp( modFitgam, scale=F)
# plot(Impgam, top=20)

#Best iteration
modBest <- modFitgam$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitgam$times$final[3]
#Samples
samp <- dim(modFitgam$resample)[1]

# Time difference of 3.021874 hours
# Accuracy : 0.4693          
# 95% CI : (0.4405, 0.4981)




#--------------------------------- 
#---------------------- XGB (new version CARET)
#--------------------------------- 

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
bootControl <- trainControl(number=25, verboseIter=TRUE) 


xgbGrid <- expand.grid(
  eta = 0.1112,
  max_depth = 71,
  nrounds = 300,
  gamma = 0.501,
  colsample_bytree = 0.201,
  min_child_weight = 0.01
)
#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight
#eta (0,1) - default: 0.3
#max_depth (1-Inf) - default: 6
#gamma (0-Inf) - default: 0
#min_child_weight (0-Inf) - default: 1
#colsample_bytree (0-1) - default:1


modFitxgb <-  train(
  festatus ~ .,
  # x = trainDat[, 1:(ncol(trainDat)-1)],
  # y = trainDat[, ncol(trainDat)], 
  data = trainDat,
  trControl = bootControl,
  metric = "Accuracy",
  method = "xgbTree",
  tuneGrid = xgbGrid,
  verbose = 1,
  num_class = 3
)

modFitxgb

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


#-------  EXECUTION COMMENTS


# xgbGrid <- expand.grid(
#   eta = 0.1112,
#   max_depth = 71,
#   nrounds = 300,
#   gamma = 0.501,
#   colsample_bytree = 0.201,
#   min_child_weight = 0.01
# )
# > modBest <- modFitxgb$bestTune; modBest
# nrounds max_depth    eta gamma colsample_bytree min_child_weight
# 1     300        71 0.1112 0.501            0.201             0.01
# Accuracy : 0.8057          
# 95% CI : (0.7998, 0.8115)
# Time difference of 39.0723 mins


# xgbGrid <- expand.grid(
#   eta = 0.1112,
#   max_depth = 71,
#   nrounds = seq(100,1000, length.out=10),
#   gamma = 0.501,
#   colsample_bytree = 0.201,
#   min_child_weight = 0.01
# )
# > modBest <- modFitxgb$bestTune; modBest
# nrounds max_depth    eta gamma colsample_bytree min_child_weight
# 3     300        71 0.1112 0.501            0.201             0.01
# Accuracy : 0.8068          
# 95% CI : (0.8009, 0.8126)
# Time difference of 41.65406 mins



# xgbGrid <- expand.grid(
#   eta = seq(0.0001, 1, length.out=10 ),
#   max_depth = 71,
#   nrounds = 400,
#   gamma = 0.501,
#   colsample_bytree = 0.201,
#   min_child_weight = 0.01
# )
# > modBest <- modFitxgb$bestTune; modBest
# nrounds max_depth    eta gamma colsample_bytree min_child_weight
# 2     400        71 0.1112 0.501            0.201             0.01
# Accuracy : 0.8064          
# 95% CI : (0.8005, 0.8122)
# Time difference of 3.668155 hours

# xgbGrid <- expand.grid(
#   eta = 0.12,
#   max_depth = seq(1,110,10),
#   nrounds = 400,
#   gamma = 0.501,
#   colsample_bytree = 0.201,
#   min_child_weight = 0.01
# )
# > modBest <- modFitxgb$bestTune; modBest
# nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 8     400        71 0.12 0.501            0.201             0.01
# Accuracy : 0.8052         
# 95% CI : (0.7993, 0.811)
# Time difference of 1.658434 hours


# xgbGrid <- expand.grid(
#   eta = 0.12,
#   max_depth = 10,
#   nrounds = 400,
#   gamma = seq(0.001,5.5,0.5),
#   colsample_bytree = 0.201,
#   min_child_weight = 0.01
# )
# > modBest <- modFitxgb$bestTune; modBest
# nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 2     400        10 0.12 0.501            0.201             0.01
# Accuracy : 0.8044          
# 95% CI : (0.7985, 0.8102)
# Time difference of 26.86071 mins



# xgbGrid <- expand.grid(
#   eta = 0.12,
#   max_depth = 10,
#   nrounds = 400,
#   gamma = 1,
#   colsample_bytree = 0.201,
#   min_child_weight = seq(0.01,1100,100)
# )
# nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 1     400        10 0.12     1            0.201             0.01
# Accuracy : 0.8046          
# 95% CI : (0.7987, 0.8104)
# Time difference of 18.95924 mins

# xgbGrid <- expand.grid(
#   eta = 0.12,
#   max_depth = 10,
#   nrounds = 400,
#   gamma = 1,
#   colsample_bytree = seq(0.001,1,0.1),
#   min_child_weight = 1
# )
# > modBest <- modFitxgb$bestTune; modBest
# nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 3     400        10 0.12     1            0.201                1
# Accuracy : 0.8037          
# 95% CI : (0.7978, 0.8095)
# Time difference of 43.83784 mins

# xgbGrid <- expand.grid(
#   eta = 0.12,
#   max_depth = 10,
#   nrounds = 400,
#   gamma = 1,
#   colsample_bytree = seq(1,100,50),
#   min_child_weight = seq(1,100,50)
# )
# Accuracy : 0.8121         
# 95% CI : (0.7887, 0.834)
# Time difference of 13.9649 hours


#------ 2015-12-06

# xgbGrid <- expand.grid(
#   eta = 0.12,
#   max_depth = 10,
#   nrounds = 400,
#   gamma = seq(1,500,100),
#   colsample_bytree = 1,
#   min_child_weight = 1
# )
# Accuracy : 0.8104         
# 95% CI : (0.787, 0.8324)
# Time difference of 1.066812 hours