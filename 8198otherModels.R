
#--------------------------------- 
#---------------------- MARS - SPLINES
#--------------------------------- 
library(caret)
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
#bootControl <- trainControl(method='oob', number=5, verboseIter=TRUE) 
bootControl <- trainControl(method='none',number=5, verboseIter=TRUE) 

modGrid <- expand.grid(
                        .max.num.rule = 5,
                        .popu.size = 10,
                        .max.gen = 10
                       )

modFitmod <-  train(
  #festatus ~ .,
   x = trainDat[, 1:(ncol(trainDat)-1)],
   y = trainDat[, ncol(trainDat)],
  data = trainDat,
  trControl = bootControl,
  tuneGrid = modGrid,
  method = "FH.GBML",
  trace = TRUE
)

predmod <- predict( modFitmod, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatmod <- confusionMatrix(testDat$festatus, predmod); conMatmod 
conMatmoddf <- as.data.frame(conMatmod$overall); modAcc <- conMatmoddf[1,1]; modAcc <- as.character(round(modAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(modGrid) < 2  )  { resampleHist(modFitmod) } else  
{ plot(modFitmod, as.table=T) }

#Best iteration
modBest <- modFitmod$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitmod$times$final[3]
#Samples
samp <- dim(modFitmod$resample)[1] ; samp
numvars <- ncol(trainDat); numvars

#Variable Importance
Impmod <- varImp( modFitmod, scale=F)
plot(Impmod, top=(ncol(trainDat)-1))

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitmod,
  file=paste("con_",numvars,"vars_gcv_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",modAcc,"__.RData", sep="")
)

