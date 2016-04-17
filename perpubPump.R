

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

#Remove num_private
#datIncl$num_private <- datIn$num_private
#datIncl <- datIncl[,c(1:30,32,31)] #reorder

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
lonlatgps$district_code <- ifelse(lonlatgps$district_code==0, NA, lonlatgps$district_code)
lonlatgps$latitude <- ifelse(lonlatgps$latitude==-2e-08, NA, lonlatgps$latitude)
lonlatgps$longitude <- ifelse(lonlatgps$longitude==0, NA, lonlatgps$longitude)
lonlatgps$gps_height <- ifelse(lonlatgps$gps_height==0, NA, lonlatgps$gps_height)
lonlatgps$amount_tsh <- ifelse(lonlatgps$amount_tsh==0, NA, lonlatgps$amount_tsh)
lonlatgps$population <- ifelse(lonlatgps$population==0, NA, lonlatgps$population)

library(missForest)
lonlatgps$festatus <- datIncl$festatus
lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
#save(lolagpnona, file="lolagpnona_missForest_All.RData")
load("lolagpnona_missForest_All.RData")

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



# #Checking Plot: Imputed longitued in red, the rest in blue.
# datplot <- data.frame(lonori=datIn$longitude, latori=datIn$latitude, lonimp=datIncl$longitude, latimp=datIncl$latitude)
# datplot$col <- ifelse(datplot$lonori==0, "red", "blue")
# datplot$exp <- ifelse(datplot$lonori==0, 0.8, 0.5)
# datplot$tip <- ifelse(datplot$lonori==0, "+", ".")
# plot(datplot$lonimp, datplot$latimp, col=datplot$col, cex=datplot$exp,
#      pch=datplot$tip, type="p", xlab="Longitude", ylab="Latitude"
#      ,main="Longitudes original (blue) and imputed (red)")
# # plot(datIncl$longitude, datIncl$latitude, cex=0.5, pch=".", type="p")
# # plot(datIn$longitude, datIn$latitude, cex=0.5, pch=".", type="p", xlim=c(30,41))


# Kind of distance based on longitude/latitude
library(geosphere)
#datIncl$fedist <- sqrt(datIncl$longitude^2 + datIncl$latitude^2)
datIncl$fedist <- distGeo(as.matrix(datIncl[,c('longitude','latitude')]), c(0,0))
# Kind of utilization factor population/#pumps by region
DF <- as.data.table(datIncl)
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
impTmp <- merge(datIncl, nonaend, by.x="region", by.y='region')
datIncl <- impTmp
datIncl <- datIncl[, c(1:30, 32:36, 31)] #reorder datIncl

#Treat NAs in permit - public_meeting
perpub <- data.frame(permit=datIncl$permit, public_meeting=datIncl$public_meeting, festatus=datIncl$festatus)
newpermit <- ifelse(is.na(perpub$permit), NA, ifelse(perpub$permit=="TRUE",1, 0))
newpubmet <- ifelse(is.na(perpub$public_meeting), NA, ifelse(perpub$public_meeting=="TRUE",1, 0))
perpub$permit <- newpermit
perpub$public_meeting <- newpubmet
#permit/public_management imputed  - gbmC it does not work...
library(imputeR)
perpubnona <- as.data.frame(impute(perpub, cFun="rpartC", verbose=FALSE)$imp) 

datIncl$permit <- perpubnona$permit
datIncl$public_meeting <- perpubnona$public_meeting

#Clean workspace
rm(DF, impTmp, nona, nonadf,nonaend, nonatmp, perpub, perpubnona, poppumre, cltmp, i, newpermit, newpubmet,nonanona)
rm(lolagpnona, lonlatgps)

#Add region_code and district_code both are numeric
datIncl$region_code <- datIn$region_code
datIncl$district_code <- datIn$district_code
#datIncl$num_private <- datIn$num_private
#datIncl <- datIncl[,c(1:33, 35:36, 34)] #reorder
#remove public_meeting not important...
#datIncl <- datIncl[ -c(9)]




#Transform factors in numeric 
for( i in 1:(ncol(datIncl)-1)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "factor") {
    datIncl[,i] <- as.numeric( datIncl[,i] )
  } else next
}


# 
# #QualityGroup to Numeric
# quagrnum <- rownames(as.data.frame(sort(table(datIncl$quality_group),decreasing=TRUE)))
# quagrnumDF <- data.frame(quagrnum, val=1:length(quagrnum))
# quagrori <- data.frame(id=datIncl$id, quality_group=datIncl$quality_group)
# quagrmerge <- merge(quagrori, quagrnumDF, by.x="quality_group", by.y="quagrnum", sort=FALSE)
# quagrnumsort <- merge(quagrori, quagrmerge, by.x="id", by.y="id", sort=FALSE)
# fequagrnum <- quagrnumsort[, c(4)]
# rm(quagrnum, quagrnumDF, quagrori, quagrmerge, quagrnumsort)
# 
# #WaterQuality to Numeric
# watgrnum <- rownames(as.data.frame(sort(table(datIncl$water_quality),decreasing=TRUE)))
# watgrnumDF <- data.frame(watgrnum, val=1:length(watgrnum))
# watgrori <- data.frame(id=datIncl$id, water_quality=datIncl$water_quality)
# watgrmerge <- merge(watgrori, watgrnumDF, by.x="water_quality", by.y="watgrnum", sort=FALSE)
# watgrnumsort <- merge(watgrori, watgrmerge, by.x="id", by.y="id", sort=FALSE)
# fewatgrnum <- watgrnumsort[, c(4)]
# rm(watgrnum, watgrnumDF, watgrori, watgrmerge,watgrnumsort)
# 
# datIncl$fequagrnum <- fequagrnum
# datIncl$fewatgrnum <- fewatgrnum
# datIncl <- datIncl[, c(1:34, 36:37, 35)]
# 
# 
# #Just the non-factor columns
# colgod <- vector()
# cnt <- 0
# for( i in 1:ncol(datIncl)) {
#   cltmp <- class(datIncl[, i])
#   if(cltmp != "factor") {
#     cnt <- cnt+1
#     colgod[cnt] <- i
#   } else next
# }
# 
# datIncl <- datIncl[, c(colgod, ncol(datIncl))]

# library(stringr)
# newfestatus <- str_replace_all(datIncl$festatus, "functional_needs_repair", "non_functional")
# datIncl$festatus <- as.factor(newfestatus)


# library(DMwR)
# datsmote <- SMOTE(festatus ~. , datIncl, perc.over=900, perc.under=200)
# prop.table(table(datsmote$festatus))*100
# datIncl <- datsmote
# #rm(datsmote)

library(caret)
set.seed(1)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]




#--------------------------------- 
#---------------------- XGB
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(method="oob", number=5, verboseIter=TRUE) 


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
#   .eta = seq(0.01, 0.05,0.01),
#   .max_depth = 12,
#   .nrounds = 300
# )

xgbGrid <- expand.grid(
  .eta = 0.04,
  .max_depth = 12,
  .nrounds = 300
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

#Variable Importance
# Impxgb <- varImp( modFitxgb, scale=F)
# plot(Impxgb, top=20)

#Best iteration
modBest <- modFitxgb$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitxgb$times$final[3]
#Samples
samp <- dim(modFitxgb$resample)[1] ; samp
numvars <- ncol(trainDat); numvars

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitxgb,
  #file=paste("xgb_17var_3class_samp100_n",samp,"_grid",modBestc,"_Downbalan__",xgbAcc,"__.RData", sep="")
  file=paste("concurso_",numvars,"vars_xgb_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",xgbAcc,"__.RData", sep="")
)

comentaXgb <- function() {

  #---- All wrong variables imputed with "missForest".
  #--- No num_private - Imputed long/lat - gps_height - amount_tsh
  #--- New variables : region_code, district_code
  #--- New variables : dist + numbpump region + people/pump  + public/permit
  #Data: 35!! variables - 3 classes - 100 sample - not balanced
  #Data: New formula for "dist" (geodesic) 
  #Expe: Basic variables, new imputation. Grid: 300,12,0.04.
  #Summ: With new Impute -> 84% !! (will it be true??) - Error ~ 8.8%. The lowest so far!! (will it be true??)
  #New:: All variables with this new imputation system...??
  #Rest: 83.87%. 
  #ExTm: 16.86min
  # Accuracy : 0.8387         
  # 95% CI : (0.8332, 0.844)
  
  #---------------------------
  #------ 2015-10-05 - Back to basic
  #--------------------------- 
  
  
#--- New variables:  installer, funder, wpt_name, subvillage, ward, scheme_name
#--- New variables : lga, region_code, district_code
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 38 variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 1
#Summ: It improves and CI is in the max. range to win...
#New:: Improve in any parameter
#Rest: 81.10%%. 
#ExTm:  15.16mins.
#Accuracy : 0.811           
#95% CI : (0.8052, 0.8168)

#--- New variables: lga
#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 38 variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap *5* - check what happens with lga included... It improves.
#Summ: Exactly the same values... no improvement...
#Next: bootstraps 25.
#Rest: 80.67%
#ExTm: 19.50mins
# Accuracy : 0.8067          
# 95% CI : (0.8008, 0.8124)

#--- New variables: lga
#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 38 variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap *5* - check what happens with lga included... It improves.
#Summ: Maximum value in 0.04
#Next: bootstraps 25.
#Rest: 80.67%
#ExTm: 19.50mins

#---------------------------
#------ 2015-09-29 - LGA ----
#---------------------------
#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 5 - Best: 300 - 0.04 - 12
#Summ: Maximum value in 0.04
#Rest: 80.37%
#ExTm: 55.94mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 2 - Best: 300 - 0.05 - 12
#Summ: It improves with lower values of eta. It can do it better...
#Summ: High values of 0.05 and 0.07 but lower for 0.06. What run with 0.05 and 0.07
#Rest: 80.35%
#ExTm: 34.22mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 2 - Best: 300 - 0.07 - 12
#Summ: It improves with lower values of eta. It can do it better...
#Rest: 80.49%
#ExTm: 31.75mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 2 - Best: 300 - 0.09 - 12
#Summ: It does not improve with max_tree
#Rest: 80.38%
#ExTm: 22.15mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 1 - 300 - 0.09 - 11
#Summ: It is not better than RF... 
#Rest: 80.46%
#ExTm: 14.27mins

#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 4 - Best: 300 - 0.09 - 13 
#Summ: It does not improve...
#Rest:  80.67%. 
#ExTm:  20.39mins

#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 4 - Best: 300 - 0.09 - 13 
#Summ: It improves with a lower .eta. 
#Rest:  80.67%. 
#ExTm:  20.74mins

#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 4 - Best: 300 - 0.11 - 13 
#Summ:  Good Result, it can be optimized a little bit with lower than 0.11
#Rest: 80.57%. 
#ExTm:  20.74mins

#-------
#------- All numeric
#-------


#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 2 (eta: 11/13): Best: 13, 300, 0.13 .
#Summ: It does not improve but the combined search has improved...
#Rest: 80.67%. It is good, but in platform... ?? (typically gets lower values!..)
#ExTm: 28.05 mins.


#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 1 - 13, 300, 0.13 just to try.
#Summ: Let see if it can improve...
#Rest: 80.49%. 
#ExTm: 18.54 mins.


#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 2 - Best: 13, 300, 0.13 
#Summ: it does not improve...
#Rest: 80.32%.
#ExTm: 20.32 mins
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 2 - Best: 13, 300, 0.11 
#Summ: Reduced max_depth - By chance!! 80.90%. Let's try with other .eta
#Rest: 80.90%.
#ExTm: 21.52 mins
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 2 - Best: 13, 300, 0.11 
#Summ: Reduced max_depth - Good starting...
#Rest: 80.43%.
#ExTm: 20.6 mins
}

#--------------------------------- 
#---------------------- RF
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

rfGrid <- expand.grid(.mtry=seq(3,7,1))
#rfGrid <- expand.grid(.mtry=4)

modFitrf <-  train(
  festatus ~ .,
  data = trainDat,
#   x = trainDat[, c(1:(ncol(trainDat)-1)) ],
#   y = trainDat[, ncol(trainDat) ],
  method = "rf",
  trControl = bootControl,
  tuneGrid = rfGrid,
  do.trace = TRUE,
  ntree =  200
)

# library(party)
# modFitrf <- cforest(festatus ~., data=trainDat, control = cforest_control(mtry=4))

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
  file=paste("concurso_",numvars,"vars_rf_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",rfAcc,"__.RData", sep="")
)

comentaRF <- function() {
#The most important variables are "numeric".
#RF separates better with them: latitude, fedist, longitude, offset_days, id, gps_heights, population...
#To include more variables numeric. 
#To transform factor to numeric in some way: "waterpoint_type" and "quantity" are the most relevant.
# SMOTE does not improve at all the score.


# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 36!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: 
# Rest: 81.31%.
# ExTm: 25.78min.

# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: Change to lassoR. Remove and put again public_meeting. Remove num_private. And gets 81.41%
# Rest: 81.41%.
# ExTm: 4.74min.

# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: Change to "ridgeR" for imputation... It gets worse...Hmm!..
# Rest: 81.28%.
# ExTm: 4.83hours.

# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: More ntrees=500 and boostraps:50
# Rest: 81.37%.
# ExTm: 1.36hours.

# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: Explore grid. Best mtry=4. But it does not improve.
# Rest: 81.40%.
# ExTm: 21.17mins


# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: Imputed population!...
# Rest: 81.40%.
# ExTm: 4.61mins


# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: As "cforest" it does not improve 76.5%76.5%. Extremely big object: 1.9Gb.
# Rest: 76.5%.
# ExTm: 4.25mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: change cutoff does not improve. Do not touch...
#Rest: 62.20% / 70%
#ExTm: 4.25mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: increase nodesize=5, it does not improve...
#Rest: 81.17%
#ExTm: 5.90mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: cross-validated - 5 reps - mtry=4. 
#Summ: It does not improve.
#Rest: 81.15%
#ExTm: 23.09mins

#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 1 - mtry=4  - More ntrees=500
#Summ: It does not improve.
#Rest: 81.38%
#ExTm: 9.942mins


#--- Also imputed amount_tsh no-ceroes...
#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceroes.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 1 - mtry=4  
#Summ: It improved very much!. "amount_tsh" has improved a lot 7th place.  
#Rest: 81.42%
#ExTm: 4.22mins


#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceros.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *10*, tuneGrid 1 - mtry=4  
#Summ: With more bootstraps barely improves... Important region_code/district_code are a important variable
#Rest: 81.29%
#ExTm: 12.14mins


#--- Put in again region_code, district_code to see what happens.
#--- With longitude and gps_height imputed... no-ceros.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 5 (3:7) - mtry=4  
#Summ: It improves very slightly...
#Rest: 81.27%
#ExTm: 15.17 mins


#--- With longitude and gps_height imputed... no-ceros.
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 5 (3:7) - mtry=4  
#Summ: It does not improve...
#Rest: 81.26%
#ExTm: hours

#--- What if, change again to just *two classes* sacrificing "needs_repair"...
#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *50*, tuneGrid 1 - mtry=5 - 
#Summ: It improves but not enough to count for the sistematic error...
#Rest: 82.61%. (but it will have an error of 0.0719) -> expected value of 0.7542
#ExTm: 2.47hours

#--- Remove : num_private!!.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *100*, tuneGrid 1 - mtry=5
#Summ: It improves a little bit. Leave in that way...
#Rest: 81.31%. 
#ExTm: 1.28hours

#--- Remove : region_code, district_code
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *200*, tuneGrid 1 - mtry=5
#Summ: It does not improve... 
#Rest: 81.27%. 
#ExTm: 2.67hours


#--- Remove : region_code, district_code
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
#Summ: It improves... (no region_code/district_code) !!!! 
#Rest: 81.42%. 
#ExTm: 1.51hours


#--- New variables : region_code, district_code
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
#Summ: It improves a little bit...(1000?)...
#Rest: 81.15%. 
#ExTm: 1.46hours


#--- New variables : region_code, district_code
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *50*, tuneGrid 1 - mtry=5
#Summ: It does not improve.
#Rest: 81.09%. 
#ExTm: 33.9 mins.

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

#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 4 - mtry=4-10 - Best: 4
#Summ:  New Record !. 
#Rest: 81.28%. 
#ExTm: 12.42 mins


#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 4 - mtry=4-10 - Best: 4
#Summ:  Record !. Let's see between 2 to 5...
#Rest: 81.14%. 
#ExTm: 13.65 mins

#-------
#------- All numeric
#-------


#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 2 - mtry=18 and 20. 
#Summ: It does not improve... with 18 is lower than with 20.
#Rest: 80.85%. 
#ExTm: 1.07hours

#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 1 - mtry=18 
#Summ: Error in the execution... To Repeat again...
#Rest: 80.85%. 
#ExTm: 37mins

#--- New variables : Just num_private.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) 
#Expe: bootstrap: *5*, tuneGrid 2 - mtry=20 and 24. Best with 20.  
#Summ: With 20 is the best. Reduce it...?.
#Rest: 80.85%. 
#ExTm: 1.25hours

#--- New variables : num_private + SMOTE.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 35!! variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) + SMOTE!! + num_private
#Expe: bootstrap: *5*, tuneGrid 1 - mtry=25. 
#Summ: It works better with mtry=28. varImp: days, fedist, long, lat, water_quality...
#Rest: 92.67%. 
#ExTm: 2.05hours


#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 2 - mtry=25. 
#Summ: It improved a bit, but can not go any longer.. Try with just one mtrh=28.
#Rest: 80.86%. 
#ExTm: 1.18hours

#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 3 - mtry=25. 
#Summ: It improved. And can continue growing more..more...  
#Rest: 80.82%. It seems it is saturating..
#ExTm: 1.16hours

#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 3 - mtry=20. 
#Summ: It improved. And can continue growing more..more...  
#Rest: 80.78%.
#ExTm: 1.24hours

#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 3 - mtry=15. 
#Summ: It improves a lot. And can continue growing more..more...  
#Rest: 80.41%.
#ExTm: 44.8mins.

#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid 5 - mtry=10
#Summ: it can continue improving.
#Rest: 79.63%.
#ExTm: 1.89 hours.
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
datTesting$num_private <- datTestingori$num_private



#--------------------------------- 
#--------------------- Feature Engineering
#--------------------------------- 

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


library(missForest)
lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
save(lolagpnona, file="lolagpnona_missForest_All_Upload.RData")
#load("lolagpnona_missForest_All.RData")

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



#Checking Plot: Imputed longitued in red, the rest in blue.
datplot <- data.frame(lonori=datTestingori$longitude, latori=datTestingori$latitude, lonimp=datTesting$longitude, latimp=datTesting$latitude)

datplot$col <- ifelse(datplot$lonori==0, "red", "blue")
datplot$exp <- ifelse(datplot$lonori==0, 0.8, 0.5)
datplot$tip <- ifelse(datplot$lonori==0, "+", ".")
plot(datplot$lonimp, datplot$latimp, col=datplot$col, cex=datplot$exp,
     pch=datplot$tip, type="p", xlab="Longitude", ylab="Latitude"
     ,main="Longitudes original (blue) and imputed (red)")
# plot(datIncl$longitude, datIncl$latitude, cex=0.5, pch=".", type="p")
# plot(datIn$longitude, datIn$latitude, cex=0.5, pch=".", type="p", xlim=c(30,41))



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

nameData <- c("concurso_36vars_rf_3class_samp100_n5_grid4_nobalan__81.42__")
load(paste(nameData,".RData",sep=""))
predConcurso_mod <- as.vector(predict(modFitrf, newdata=datTesting))
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
tabEnd <- rbind(trainTab,testTab, datSubTab)
rownames(tabEnd) <- c('train','test','datSub')
tabEnd

timval <- str_replace_all(Sys.time(), " |:", "_")
write.table(datSuboend, file=paste("Res_xxxx_",nameData,timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

rm(datSubo, trainTab, testTab, datSubTab, tabEnd, datId, datTmp)



#--------------------------------- 
#---------------------- rFERNS
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=50, verboseIter=TRUE) 

rfernGrid <- expand.grid(.depth=seq(5,10,1))
#rfernGrid <- expand.grid(.depth=12)

modFitrfern <-  train(
  x = trainDat[, 1:(ncol(trainDat)-1)],
  y = trainDat[, ncol(trainDat)],
#   festatus ~ .,
#   data = trainDat,
  method = "rFerns",
  trControl = bootControl,
  tuneGrid = rfernGrid
)


predrfern <- predict( modFitrfern, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatrfern <- confusionMatrix(testDat$festatus, predrfern); conMatrfern 
conMatrferndf <- as.data.frame(conMatrfern$overall); rfernAcc <- conMatrferndf[1,1]; rfernAcc <- as.character(round(rfernAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(rfernGrid) < 2  )  { resampleHist(modFitrfern) } else  
{ plot(modFitrfern, as.table=T) }

# #Variable Importance
Imprfern <- varImp( modFitrfern, scale=F)
plot(Imprfern, top=20)

#Best iteration
modBest <- modFitrfern$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitrfern$times$final[3]
#Samples
samp <- dim(modFitrfern$resample)[1] ; samp
numvars <- ncol(trainDat); numvars

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitrfern,
  file=paste("concurso_",numvars,"vars_rfern_3class_samp100_n",samp,"_grid",modBestc,"_SMOTE__",rfernAcc,"__.RData", sep="")
)

#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) + SMOTE!!.
#Expe: bootstrap: *5*, tuneGrid 4 - Best: depth=16.
#Summ: It can grow a little bit... Super-fast. Models are super-huge... 1.5Gb.!!
#Rest: 84.56%. 
#ExTm: 15.81mins.
#--- New variables : dist + numbpump region + people/pump  + public/permit
#Data: 34 variables - 3 classes - 100 sample - not balanced
#Data: New formula for "dist" (geodesic) + SMOTE!!.
#Expe: bootstrap: *5*, tuneGrid 7 - Best: depth=14.
#Summ: It can grow a little bit... Super-fast.
#Rest: 83.15%. 
#ExTm: 15.58mins.


cvValues <- resamples(
  list(
    RF = modFitrf, 
    XGB = modFitxgb,
    RFERN = modFitrfern
  )
)
dotplot(cvValues)
modelCor(cvValues)
summary(cvValues)

#---------------
a <- names(datIncl)
b <- names(datIn)
setdiff(b,a)

# Variables
# amount_tsh -                Total static head (amount water available to waterpoint)
# date_recorded -             The date the row was entered
# funder -                    Who funded the well
# gps_height -                Altitude of the well
# installer -                 Organization that installed the well
# longitude -                 GPS coordinate
# latitude -                  GPS coordinate
# wpt_name -                  Name of the waterpoint if there is one
# num_private -
# basin -                     Geographic water basin
# subvillage -                Geographic location
# region -                    Geographic location
# region_code -               Geographic location (coded)
# district_code -             Geographic location (coded)
# lga -                       Geographic location
# ward -                      Geographic location
# population -                Population around the well
# public_meeting -            True/False
# recorded_by -               Group entering this row of data
# scheme_management -         Who operates the waterpoint
# scheme_name -               Who operates the waterpoint
# permit -                    If the waterpoint is permitted
# construction_year -         Year the waterpoint was constructed
# extraction_type -           The kind of extraction the waterpoint uses
# extraction_type_group -     The kind of extraction the waterpoint uses
# extraction_type_class -     The kind of extraction the waterpoint uses
# management -                How the waterpoint is managed
# management_group -          How the waterpoint is managed
# payment -                   What the water costs
# payment_type -              What the water costs
# water_quality -             The quality of the water
# quality_group -             The quality of the water
# quantity -                  The quantity of water
# quantity_group -            The quantity of water
# source -                    The source of the water
# source_type -               The source of the water
# source_class -              The source of the water
# waterpoint_type -           The kind of waterpoint
# waterpoint_type_group -     The kind of waterpoint

# Todas "functional":               0.5461
# Todas "non functional":           0.3820
# Todas "functional needs repair":  0.0719


#--------------------------------- 
#---------------------- NNET
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

nnetGrid <- expand.grid(
                         .size=seq(1,11,5),
                         .decay=c(0, 0.1, 1, 2)
                       )

modFitnnet <-  train(
  festatus ~ .,
  data = trainDat,
  #   x = trainDat[, c(1:(ncol(trainDat)-1)) ],
  #   y = trainDat[, ncol(trainDat) ],
  method = "nnet",
  preProc = c('center', 'scale'),
  trControl = bootControl,
  trace= TRUE,
  maxit = 2000,
  tuneGrid = nnetGrid
)

# library(party)
# modFitnnet <- cforest(festatus ~., data=trainDat, control = cforest_control(mtry=4))

prednnet <- predict( modFitnnet, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatnnet <- confusionMatrix(testDat$festatus, prednnet); conMatnnet 
conMatnnetdf <- as.data.frame(conMatnnet$overall); nnetAcc <- conMatnnetdf[1,1]; nnetAcc <- as.character(round(nnetAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(nnetGrid) < 2  )  { resampleHist(modFitnnet) } else  
{ plot(modFitnnet, as.table=T) }

#Best iteration
modBest <- modFitnnet$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitnnet$times$final[3]
#Samples
samp <- dim(modFitnnet$resample)[1] ; samp
numvars <- ncol(trainDat); numvars

#Variable Importance
Impnnet <- varImp( modFitnnet, scale=F)
plot(Impnnet, top=(ncol(trainDat)-1))

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitnnet,
  file=paste("concurso_",numvars,"vars_nnet_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",nnetAcc,"__.RData", sep="")
)

# --- It was not imputed "population"!!.
# --- Also imputed amount_tsh no-ceroes...
# --- Put in again region_code, district_code to see what happens.
# --- With longitude and gps_height imputed... no-ceroes.
# --- Remove : num_private!!.
# --- New variables : dist + numbpump region + people/pump  + public/permit
# Data: 35!! variables - 3 classes - 100 sample - not balanced
# Data: New formula for "dist" (geodesic) 
# Expe: Grid with size and decay. Best: size=11, decay=1.
# Expe: It does not improve more than 0.75...
# Rest: 74.7%.
# ExTm: 30min.
