
#----------------------------------------------------
# WORKING DIRECTORY
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

# To fill long/lat, gps_height... first to include region_code/district_code
# they will act as a reference (they have no ceros) for the rest of the columns to impute.
#Add region_code and district_code both are numeric
datIncl$region_code <- datIn$region_code
datIncl$district_code <- datIn$district_code


#---------- Fill ceros in longitude and gps_height
# longitutde has 1812 ceros.sum(datIn$longitude==0)
# latitude has no ceros...sum(datIn$latitude==0)
# gps_height hast 20438 ceros.sum(datIn$gps_height==0)
# Lets fill longitude and gps_height ceros using latitude.
lonlatgps <- datIncl[, c('latitude', 'longitude','gps_height', 'amount_tsh')]

# change 0 for NAs.
lonlatgps$latitude <- ifelse(lonlatgps$latitude==-2e-08, NA, lonlatgps$latitude)
lonlatgps$longitude <- ifelse(lonlatgps$longitude==0, NA, lonlatgps$longitude)
lonlatgps$gps_height <- ifelse(lonlatgps$gps_height==0, NA, lonlatgps$gps_height)
lonlatgps$amount_tsh <- ifelse(lonlatgps$amount_tsh==0, NA, lonlatgps$amount_tsh)

# It takes around 60 minutes - 10 min by iteration (5 iterations).
library(missForest)
lonlatgps$festatus <- datIncl$festatus
lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
save(lolagpnona, file="lolagpnona.RData")
load("lolagpnona_w_lati.RData")

#library(imputeR)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="lassoR", verbose=FALSE)$imp)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="ridgeR", verbose=FALSE)$imp)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="glmboostR", verbose=TRUE)$imp)

datIncl$longitude <- lolagpnona$longitude
datIncl$gps_height <- lolagpnona$gps_height
datIncl$amount_tsh <- lolagpnona$amount_tsh


# Kind of distance based on longitude/latitude
library(geosphere)
#datIncl$fedistCar <- sqrt(datIncl$longitude^2 + datIncl$latitude^2)
datIncl$fedist <- distGeo(as.matrix(datIncl[,c('longitude','latitude')]), c(0,0))
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
#datIncl <- datIncl[, c(1:30, 32:36, 31)] #reorder datIncl
#datIncl <- datIncl[, c(1:36, 38:42, 37)] #reorder datIncl all variables

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

#Transform factors in numeric 
for( i in 1:(ncol(datIncl)-1)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "factor") {
    datIncl[,i] <- as.numeric( datIncl[,i] )
  } else next
}


#--------------------------------- 
#---------------------- SEPARATE TRAINING - TESTING
#--------------------------------- 
library(caret)
set.seed(1)
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
set.seed(1) 
bootControl <- trainControl(method='oob',number=50, verboseIter=TRUE) 
#bootControl <- trainControl(number=5, verboseIter=TRUE) 

#rfGrid <- expand.grid(.mtry=seq(3,7,1))
rfGrid <- expand.grid(.mtry=5)

modFitrf <-  train(
  festatus ~ .,
  data = trainDat,
  #   x = trainDat[, c(1:(ncol(trainDat)-1)) ],
  #   y = trainDat[, ncol(trainDat) ],
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
  #Todo: Imputar with "missForest"....(libro Mastering_Data) 
  
  #---- All wrong variables imputed with "missForest".
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5 and 500 ntrees - "oob" - 
  #Summ: With new Impute -> 84% !! (will it be true??) - Error ~ 8.7%. The lowest so far!! (will it be true??)
  #New:: All variables with this new imputation system...??
  #Rest: 84%. 
  #ExTm: 6.86min
  # Accuracy : 0.84            
  # 95% CI : (0.8345, 0.8454)
  
  
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5 and 500 ntrees - "oob" - New Impute!
  #Summ: It a little bit worse. But error of 14% upload.
  #New:: ??
  #Rest: 81.36%. 
  #ExTm: 5.24min
  # Accuracy : 0.8136          
  # 95% CI : (0.8078, 0.8193)
  
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *500*, tuneGrid 1 - mtry=4 and 500 ntrees - "oob".
  #Summ: It gets worse. 
  #New:: mtry=5 better, bootstraps has no effect. 
  #Rest: 81.31%. 
  #ExTm: 4.88min
  # Accuracy : 0.8131          
  # 95% CI : (0.8073, 0.8188)
  
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5 and 500 ntrees - "oob".
  #Summ: Exactly the same value in everything... Better error Upload!!.
  #New:: 
  #Rest: 81.38%. 
  #ExTm: 10min
  # Accuracy : 0.8138         
  # 95% CI : (0.808, 0.8195) 
  
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *25*, tuneGrid 4 - mtry=3-7 and 500 ntrees.
  #Summ: Best value mtry=4. 
  #New:: "oob". Just mtry=4. 
  #Rest: 81.38%. 
  #ExTm: 3.5023hours. 
  # Accuracy : 0.8138         
  # 95% CI : (0.808, 0.8195)
  
  #---------------------------
  #------ 2015-10-04 - "Back to Basics"
  # Minimum number of variables.
  #---------------------------
  
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 2 - mtry=4-6 - Best 6. "adaptive_boot".
  # Summ: A little bit worse.
  # New:: 
  # Rest: 81.4%.
  # ExTm:  2.99 hours
  # Accuracy : 0.814           
  # 95% CI : (0.8082, 0.8197)
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 2 - mtry=5-6 - Best 5. "boost632".
  # Summ: A little bit worse.
  # New:: "boot"
  # Rest: 81.45%.
  # ExTm:  36.08mins
  # Accuracy : 0.8145          
  # 95% CI : (0.8087, 0.8202)
  
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *50*, tuneGrid 2 - mtry=5-6 - Best 5. "OOB" as boot. *5000* trees.
  # Summ: A little bit worse.
  # New:: Change boostraping process.
  # Rest: 81.46%.
  # ExTm:  1.16hours.
  # Accuracy : 0.8146          
  # 95% CI : (0.8088, 0.8203)
  
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *50*, tuneGrid 1 - mtry=5. "OOB" as boot. 500 trees.
  # Summ: Worse.
  # New:: 
  # Rest: 81.59%.
  # ExTm:  10.25mins.
  # Accuracy : 0.8159          
  # 95% CI : (0.8102, 0.8216)
  
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *50*, tuneGrid 1 - mtry=5. "OOB" as boot. 500 trees.
  # Summ: Worse.
  # New:: 
  # Rest: 81.59%.
  # ExTm:  10.25mins.
  # Accuracy : 0.8159          
  # 95% CI : (0.8102, 0.8216)
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 3 - mtry=5. "OOB" as boot. 500 trees.
  # Summ: Worse.
  # New:: mtry: 5-7
  # Rest: 81.59%.
  # ExTm:  10.25mins.
  # Accuracy : 0.8159          
  # 95% CI : (0.8102, 0.8216)
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 1 - mtry=4. "OOB" as boot. 700 trees.
  # Summ: Worse... Total error: 13.93%
  # New:: OOB - 600 trees?? 
  # Rest: 81.53%.
  # ExTm:  13.61mins.
  # Accuracy : 0.8153          
  # 95% CI : (0.8095, 0.8209)
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 3 - mtry=5. "OOB" as boot. 500 trees
  # Summ: Improves.
  # New:: OOB - 700 trees?? 
  # Rest: 81.59%.
  # ExTm:  9.66mins.
  # Accuracy : 0.8159          
  # 95% CI : (0.8102, 0.8216)
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 3 - mtry=5. "OOB" as boot. 1000 trees.
  # Summ: A little bit worse
  # New:: OOB - 500 ??
  # Rest:  81.41%.
  # ExTm: 26.96 mins.
  # #     Accuracy : 0.8141          
  # 95% CI : (0.8083, 0.8198)
  
  # --- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  # --- New variables : lga, region_code, district_code
  # --- New variables : dist + numbpump region + people/pump  + public/permit
  # Data: 39 variables - 3 classes - 100 sample - not balanced
  # Data: New formula for "dist" (geodesic)
  # Expe: bootstrap: *5*, tuneGrid 1 - mtry=4. "OOB" as boot.
  # Summ: Very fast - 3 minutes -
  # New:: OOB - 1000 tree?? - Improves!!.
  # Rest:  81.5%.
  # ExTm:  17.18mins.
  # Accuracy : 0.815
  # 95% CI : (0.8092, 0.8207)
  
  #--- Remove variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  #--- New variables : lga, region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 39 variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *5*, tuneGrid 1 - mtry=4. "OOB" as boot.
  #Summ: Very fast - 3 minutes - 
  #New:: Check mtry.. . 
  #Rest: 81.21%%. 
  #ExTm:  3.56mins.
  # #   Accuracy : 0.8124          
  # 95% CI : (0.8066, 0.8182)
  
  
  #---------------------------
  #------ 2015-09-29 - ALL REMAINING VARIABLES: 
  # installer, funder, wpt_name, subvillage, ward, scheme_name
  #---------------------------
  
  
  #--- New variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  #--- New variables : lga, region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 38 variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *5*, tuneGrid 4:8 - Best: 4 (5 to 8 lower...)
  #Summ: It improves and CI is in the max. range to win...
  #New:: Check mtry.. . 
  #Rest: 81.61%%. 
  #ExTm:  20.17mins.
  # Accuracy : 0.8161          
  # 95% CI : (0.8103, 0.8218)
  
  
  #--- New variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
  #--- New variables : lga, region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 38 variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *5*, tuneGrid 4:8 - Best: 4 (5 to 8 lower...)
  #Summ: It improves and CI is in the max. range to win...
  #New:: Check mtry.. . 
  #Rest: 81.61%%. 
  #ExTm:  20.17mins.
  # Accuracy : 0.8161          
  # 95% CI : (0.8103, 0.8218)
  
  #---------------------------
  #------ 2015-09-29 - ALL REMAINING VARIABLES: 
  # installer, funder, wpt_name, subvillage, ward, scheme_name
  #---------------------------
  
  #--- New variables: lga
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 38 variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - 4
  #Summ: It gets worse..
  #New:: 
  #Rest: 81.25%. 
  #ExTm:  41.81mins.
  # Accuracy : 0.8125          
  # 95% CI : (0.8067, 0.8182)
  
  #--- New variables: lga
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 38 variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *25*, tuneGrid 1 - 4
  #Summ: It improves a little bit. The expected error in platform ~ 15%.... Higher.
  #New:: 50 bootstraps.
  #Rest: 81.28%. 
  #ExTm:  30.58mins.
  # Accuracy : 0.8128          
  # 95% CI : (0.807, 0.8185)
  
  
  #--- New variables: lga
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 38 variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *5*, tuneGrid 3 - 3:5  
  #Summ: 81.25%. - Best: mtry = 4. But lga one of the best variables 10th position. 
  #New:: Run mtry=4 + 25 boots.
  #Rest: 81.25%. 
  #ExTm: 19.19 mins.
  # Accuracy : 0.8125          
  # 95% CI : (0.8067, 0.8182)
  
  #---------------------------
  #------ 2015-09-29 - LGA ----
  #---------------------------
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 37!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *25*, tuneGrid 1 - mtry=5
  #Summ: Run a grid...3:5. Best in mtry=4. - 25 boostraps.
  #New:: No improvement...
  #Rest: 81.37%. 
  #ExTm: 1.18hours.
  # Accuracy : 0.8137          
  # 95% CI : (0.8079, 0.8194)
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 37!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
  #Summ: Run a grid...2:8. Best in mtry=4.
  #New:: Run a grid 3:5 but with 25 bootstraps..
  #Rest: 81.37%. 
  #ExTm:  48mins.
  # Accuracy : 0.8137          
  # 95% CI : (0.8079, 0.8194)
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 37!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
  #Summ: Run the same thing again, generating new train/test data. Improves!!.
  #New:: Create a grid.
  #Rest: 81.33%. 
  #ExTm:  48mins.
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 37!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
  #Summ: Put again num_private...
  #New::  No change...
  #Rest: 81.26%. 
  #ExTm:  47mins.
  
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
  #Summ: Remove num_private. 
  #New:: It does not improve.
  #Rest: 81.26%. 
  #ExTm: 47.66 mins.
  
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *300*, tuneGrid 1 - mtry=5
  #Summ: Repeat with 300 bootstraps.
  #New:: It does not improve.
  #Rest: 81.37%. 
  #ExTm: 3.12hours
  
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
  #Summ: Repeat with more bootstraps 
  #New:: It does not improve.
  #Rest: 81.35%. 
  #ExTm: 44.27mins
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *5*, tuneGrid 1 - mtry=5
  #Summ: Repeat the experiment and in this case improves.
  #New:: See what happens with bootstrap of 50.
  #Rest: 81.44%. 
  #ExTm: 4.34mins
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *100*, tuneGrid 4 - mtry=3-7 - Best: 5
  #Summ: It improves with the number of bootstraps. 
  #Summ: (Pending the experiment with just mtry=5 and 50 bootstraps)
  #Rest: 81.33%. 
  #ExTm: 5 hours.
  
  
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *5*, tuneGrid 4 - mtry=3-7 - Best: 5
  #Summ:  It does not improve, but keeps high...
  #Rest: 81.24%. 
  #ExTm: 17.07 mins
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

# To fill long/lat, gps_height... first to include region_code/district_code
# they will act as a reference (they have no ceros) for the rest of the columns to impute.
#Add region_code and district_code both are numeric
datTesting$region_code <- datTestingori$region_code
datTesting$district_code <- datTestingori$district_code


#---------- Fill ceros in longitude and gps_height
# longitutde has 1812 ceros.sum(datTestingori$longitude==0)
# latitude has no ceros...sum(datTestingori$latitude==0)
# gps_height hast 20438 ceros.sum(datTestingori$gps_height==0)
# Lets fill longitude and gps_height ceros using latitude.
library(imputeR)
lonlatgps <- datTesting[, c('latitude', 'longitude','gps_height', 'amount_tsh')]

# change 0 for NAs.
lonlatgps$longitude <- ifelse(lonlatgps$longitude==0, NA, lonlatgps$longitude)
lonlatgps$gps_height <- ifelse(lonlatgps$gps_height==0, NA, lonlatgps$gps_height)
lonlatgps$amount_tsh <- ifelse(lonlatgps$amount_tsh==0, NA, lonlatgps$amount_tsh)

library(missForest)
lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
#it takes 30 secs by iteration. 5 iterations.
#save(lolagpnona, file="lolagpnonaUpload.RData")
#load("lolagpnonaUpload.RData")
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="lassoR", verbose=FALSE)$imp)

datTesting$longitude <- lolagpnona$longitude
datTesting$gps_height <- lolagpnona$gps_height
datTesting$amount_tsh <- lolagpnona$amount_tsh


library(geosphere)
# datTesting$fedistCar <- sqrt(datTesting$longitude^2 + datTesting$latitude^2)
# datTesting$fedistGeo <- distGeo(as.matrix(datTesting[,c('longitude','latitude')]), c(0,0))
datTesting$fedist <- distGeo(as.matrix(datTesting[,c('longitude','latitude')]), c(0,0))
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

#nameData <- c("concurso_38vars_xgb_3class_samp100_n25_grid300_12_0.04_nobalan__80.67__"); 
#load(paste(nameData,".RData",sep=""))

library(stringr)
#modFit <- modFitrf
modFit <- modFitxgb
modtype <-modFit$method
samptmp <- modFit$resample; samp <- length(unique(samptmp$Resample))
numvars <- length(modFit$coefnames)
#scortmp <- word(nameData, 2, sep=fixed("__"))
#scortmp <- rfAcc
scortmp <- xgbAcc

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
sumEr <- round(sum(tabEnd[5,]),3)
tabEnd <- rbind(tabEnd, c(sumEr,sumEr,sumEr))
rownames(tabEnd)[6] <- "sumDiff"
tabEnd


timval <- str_replace_all(Sys.time(), " |:", "_")
#write.table(datSuboend, file=paste("Res_xxxx_",nameData,timval,".csv",sep=""),sep=","
write.table(datSuboend, file=paste("Res_xxxx_", modtype,"_",numvars,"_samp100_noban_n",samp,"_Acc_", scortmp,"_Err_",sumEr,"_",timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

rm(datSubo, trainTab, testTab, datSubTab, tabEnd, datId, datTmp)

