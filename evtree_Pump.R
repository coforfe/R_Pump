
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
#Expe: bootstrap: *5*, tuneGrid: 5 params. - Best for alpha=1
#Summ: It takes tooooo much fo the result it provides...
#Rest: 74.99%.
#ExTm: 1.62 days!!!
#---------
