


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
#---------------------- EVTREE
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

evtGrid <- expand.grid(
  .alpha = seq(1,20, 5)
)

modFitevt <-  train(
 # x = trainDat[, 1:(ncol(testDat)-1)],
  x = trainDat[, 1:(ncol(testDat)-1)],
  y = trainDat[, ncol(trainDat)],
#   festatus ~ .,
#   data = trainDat,
  method = "evtree",
  trControl = bootControl,
  tuneGrid = evtGrid
)

predevt <- predict( modFitevt, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatevt <- confusionMatrix(testDat$festatus, predevt); conMatevt 
conMatevtdf <- as.data.frame(conMatevt$overall); evtAcc <- conMatevtdf[1,1]; evtAcc <- as.character(round(evtAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(evtGrid) < 2  )  { resampleHist(modFitevt) } else  
{ plot(modFitevt, as.table=T) }

# #Variable Importance
# Impevt <- varImp( modFitevt, scale=F)
# plot(Impevt, top=20)

#Best iteration
modBest <- modFitevt$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
:cal SetSyn("r")
#Execution time:
modFitevt$times$final[3]
#Samples
samp <- dim(modFitevt$resample)[1]
numvars <- ncol(trainDat)

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitevt,
  file=paste("concurso_",numvars,"vars_evt_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",evtAcc,"__.RData", sep="")
)


#Data: 28 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid: 5 params.
#Summ: 
#Rest: %.
#ExTm: mins.
#---------

