
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
datIncl <- datIncl[, -c(11,13)] #remove permit and  public_meeting.
datIncl$festatus <- str_replace_all(datIncl$festatus, " ", "_")
datIncl <- datIncl[, -c(3,5)] #remove funder, installer, if not it does not work... It cannot be all equal.


#To transform in factors the character columns. It's very convenient.
for( i in 1:ncol(datIncl)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "character") {
    datIncl[, i] <- as.factor(datIncl[,i])
  } else next
}


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
bootControl <- trainControl(number=5, verboseIter=TRUE) 


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

xgbGrid <- expand.grid(
  .eta = seq(0.11, 0.12, 0.01),
  .max_depth = seq(10,12, 1),
  .nrounds = seq(300, 500,100 )
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


