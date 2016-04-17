
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


#To include all the remaining variables (6 more)
source("changeTrainTest.R")
tmpVal <- changeTrain()
datIncl <- cbind.data.frame(tmpVal, datIncl)


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
#Also as a reference I include region as basin as a numeric to help the imputation process.
datIncl$region <- as.numeric(datIncl$region)
datIncl$basin <- as.numeric(datIncl$basin)

#---------- Fill ceros in longitude and gps_height
# longitutde has 1812 ceros.sum(datIn$longitude==0)
# latitude has no ceros...sum(datIn$latitude==0)
# gps_height hast 20438 ceros.sum(datIn$gps_height==0)
# Lets fill longitude and gps_height ceros using latitude.
#library(imputeR)
lonlatgps <- datIncl[, c('latitude', 'longitude','gps_height', 'amount_tsh','population','region_code', 'district_code', 'basin', 'region')]
# To make better prediction indlue "ward" and "lga"  and "subvillage" from datIn although they are not included in datIncl
lonlatgps$ward <- as.numeric(as.factor(datIn$ward))
lonlatgps$lga <- as.numeric(as.factor(datIn$lga))
lonlatgps$subvillage <- as.numeric(as.factor(datIn$subvillage))

# change 0 for NAs.
lonlatgps$district_code <- ifelse(lonlatgps$latitude==0, NA, lonlatgps$district_code)
lonlatgps$latitude <- ifelse(lonlatgps$latitude==-2e-08, NA, lonlatgps$latitude)
lonlatgps$longitude <- ifelse(lonlatgps$longitude==0, NA, lonlatgps$longitude)
lonlatgps$gps_height <- ifelse(lonlatgps$gps_height==0, NA, lonlatgps$gps_height)
lonlatgps$amount_tsh <- ifelse(lonlatgps$amount_tsh==0, NA, lonlatgps$amount_tsh)
lonlatgps$population <- ifelse(lonlatgps$population==0, NA, lonlatgps$population)

library(missForest)
#lonlatgps$festatus <- datIncl$festatus
lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
#save(lolagpnona, file="lolagpnona_missForest_All.RData")
save(lolagpnona, file="lolagpnona_missForest_All_no_festatus.RData")
load("lolagpnona_missForest_All.RData")
load("lolagpnona_missForest_All_no_festatus.RData")

#library(imputeR)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="lassoR", verbose=FALSE)$imp)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="ridgeR", verbose=FALSE)$imp)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="glmboostR", verbose=TRUE)$imp)

# library(Amelia)
# lolagpnona <- amelia(lonlatgps[, 1:12 ])
# lolagpnona <- lolagpnona$imputations[[5]]
# lolagpnona <- cbind(lolagpnona$imputations$imp1, lonlatgps$festatus)
# Amelia produces values very dispersed for lon/lat, negative values for amount_tsh or population, not meaningful. Baddddd!


datIncl$district_code <- lolagpnona$district_code
datIncl$latitude <- lolagpnona$latitude
datIncl$longitude <- lolagpnona$longitude
datIncl$gps_height <- lolagpnona$gps_height
datIncl$amount_tsh <- lolagpnona$amount_tsh
datIncl$population <- lolagpnona$population



#Checking Plot: Imputed longitued in red, the rest in blue.
datplot <- data.frame(lonori=datIn$longitude, latori=datIn$latitude, lonimp=datIncl$longitude, latimp=datIncl$latitude)
datplot$col <- ifelse(datplot$lonori==0, "red", "blue")
datplot$exp <- ifelse(datplot$lonori==0, 0.8, 0.5)
datplot$tip <- ifelse(datplot$lonori==0, "+", ".")
plot(datplot$lonimp, datplot$latimp, col=datplot$col, cex=datplot$exp,
     pch=datplot$tip, type="p", xlab="Longitude", ylab="Latitude"
     ,main="Longitudes original (blue) and imputed (red)")
# plot(datIncl$longitude, datIncl$latitude, cex=0.5, pch=".", type="p")
# plot(datIn$longitude, datIn$latitude, cex=0.5, pch=".", type="p", xlim=c(30,41))


# Kind of distance based on longitude/latitude
library(geosphere)
#datIncl$fedistCar <- sqrt(datIncl$longitude^2 + datIncl$latitude^2)
datIncl$fedist <- distGeo(as.matrix(datIncl[,c('longitude','latitude')]), c(0,0))
# Kind of utilization factor population/#pumps by region
library(imputeR)
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
save(datIncl, file="datIncl_41vars.RData")
load("datIncl_41vars.RData")

#Just the important variables...
Imptmp <- Imprf[[1]]
Imptmp <- as.data.frame(Imptmp)
Impend <- data.frame(colu = rownames(Imptmp), val = Imptmp$Overall)
Impendsort <- Impend[ order(-Impend$val),]
save(Impendsort, file="Importance_Variables.RData")
load("Importance_Variables.RData")
colGod <- as.vector(Impendsort$colu[Impendsort$val > 750])

datInclSma <- datIncl[,(names(datIncl) %in% c(colGod, 'festatus')) ]
datIncl <- datInclSma


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
bootControl <- trainControl(method='oob',number=5, verboseIter=TRUE) 
#bootControl <- trainControl(number=5, verboseIter=TRUE) 

rfGrid <- expand.grid(.mtry=seq(5,8,1))
#rfGrid <- expand.grid(.mtry=5)

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
  
  #---- All variables
  #---- All wrong variables imputed with "missForest" but without "festatus".
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 3 5:8 - mtry=5 and 500 ntrees - "oob" 
  #Summ: Reduced improvement without festatus. 
  #New:: A little improvement...
  #Rest: 81.4%. 
  #ExTm: 12.55mins
  # Accuracy : 0.814           
  # 95% CI : (0.8082, 0.8197)
  
  #---- All wrong variables imputed with "missForest" but without "festatus".
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 3 5:8 - mtry=5 and 500 ntrees - "oob" 
  #Summ: Reduced improvement without festatus. 
  #New:: 
  #Rest: 81.35%. 
  #ExTm: 10.43mins
  # Accuracy : 0.8135          
  # 95% CI : (0.8077, 0.8192)
  
  #---- All wrong variables imputed with "missForest".
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: bootstrap: *50*, tuneGrid 3 4:6 - mtry=6 and 500 ntrees - "oob" - 
  #Summ: Improves with a better mtry=6. - Error: 8.20%.
  #New:: Check new mtry up...
  #Rest: 84.12%. 
  #ExTm: 8.88mins
  # Accuracy : 0.8412          
  # 95% CI : (0.8358, 0.8466)
  
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


# To fill long/lat, gps_height... first to include region_code/district_code
# they will act as a reference (they have no ceros) for the rest of the columns to impute.
#Add region_code and district_code both are numeric
datTesting$region_code <- datTestingori$region_code
datTesting$district_code <- datTestingori$district_code
#Also as a reference I include region as basin as a numeric to help the imputation process.
datTesting$region <- as.numeric(as.factor(datTesting$region))
datTesting$basin <- as.numeric(as.factor(datTesting$basin))

#---------- Fill ceros in longitude and gps_height
# longitutde has 1812 ceros.sum(datTestingori$longitude==0)
# latitude has no ceros...sum(datTestingori$latitude==0)
# gps_height hast 20438 ceros.sum(datTestingori$gps_height==0)
# Lets fill longitude and gps_height ceros using latitude.
lonlatgps <- datTesting[, c('latitude', 'longitude','gps_height', 'amount_tsh','population','region_code', 'district_code', 'basin', 'region')]
# To make better prediction indlue "ward" and "lga"  and "subvillage" from datTestingori although they are not included in datTesting
lonlatgps$ward <- as.numeric(as.factor(datTestingori$ward))
lonlatgps$lga <- as.numeric(as.factor(datTestingori$lga))
lonlatgps$subvillage <- as.numeric(as.factor(datTestingori$subvillage))

# change 0 for NAs.
lonlatgps$district_code <- ifelse(lonlatgps$latitude==0, NA, lonlatgps$district_code)
lonlatgps$latitude <- ifelse(lonlatgps$latitude==-2e-08, NA, lonlatgps$latitude)
lonlatgps$longitude <- ifelse(lonlatgps$longitude==0, NA, lonlatgps$longitude)
lonlatgps$gps_height <- ifelse(lonlatgps$gps_height==0, NA, lonlatgps$gps_height)
lonlatgps$amount_tsh <- ifelse(lonlatgps$amount_tsh==0, NA, lonlatgps$amount_tsh)
lonlatgps$population <- ifelse(lonlatgps$population==0, NA, lonlatgps$population)

# 
# library(missForest)
# lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
# save(lolagpnona, file="lolagpnona_missForest_All_Upload.RData")
load("lolagpnona_missForest_All_Upload.RData")

#library(imputeR)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="lassoR", verbose=FALSE)$imp)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="ridgeR", verbose=FALSE)$imp)
#lolagpnona <- as.data.frame(impute(as.matrix(lonlatgps), lmFun="glmboostR", verbose=TRUE)$imp)

datTesting$district_code <- lolagpnona$district_code
datTesting$latitude <- lolagpnona$latitude
datTesting$longitude <- lolagpnona$longitude
datTesting$gps_height <- lolagpnona$gps_height
datTesting$amount_tsh <- lolagpnona$amount_tsh
datTesting$population <- lolagpnona$population



# #Checking Plot: Imputed longitued in red, the rest in blue.
# datplot <- data.frame(lonori=datTestingori$longitude, latori=datTestingori$latitude, lonimp=datTesting$longitude, latimp=datTesting$latitude)
# 
# datplot$col <- ifelse(datplot$lonori==0, "red", "blue")
# datplot$exp <- ifelse(datplot$lonori==0, 0.8, 0.5)
# datplot$tip <- ifelse(datplot$lonori==0, "+", ".")
# plot(datplot$lonimp, datplot$latimp, col=datplot$col, cex=datplot$exp,
#      pch=datplot$tip, type="p", xlab="Longitude", ylab="Latitude"
#      ,main="Longitudes original (blue) and imputed (red)")
# # plot(datIncl$longitude, datIncl$latitude, cex=0.5, pch=".", type="p")
# # plot(datIn$longitude, datIn$latitude, cex=0.5, pch=".", type="p", xlim=c(30,41))


library(geosphere)
#datTesting$fedist <- sqrt(datTesting$longitude^2 + datTesting$latitude^2)
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


#To include all the remaining variables (6 more)
source("changeTrainTest.R")
valTest <- changeTest()
datTesting <- cbind.data.frame(valTest, datTesting)


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

#--------------------------------- 
#---------------------- GENERATE FILE TO UPLOAD
#--------------------------------- 

library(stringr)
modFit <- modFitrf
#modFit <- modFitxgb
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
sumEr <- round(sum(tabEnd[5,]),3)
tabEnd <- rbind(tabEnd, c(sumEr,sumEr,sumEr))
rownames(tabEnd)[6] <- "sumDiff"
tabEnd


timval <- str_replace_all(Sys.time(), " |:", "_")
#write.table(datSuboend, file=paste("Res_xxxx_",nameData,timval,".csv",sep=""),sep=","
write.table(datSuboend, file=paste("Res_xxxx_", modtype,"_",numvars,"_samp100_noban_n",samp,"_Acc_", scortmp,"_Err_",sumEr,"_",timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

rm(datSubo, trainTab, testTab, datSubTab, tabEnd, datId, datTmp)




