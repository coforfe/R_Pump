####
# BLACKBOOST 
####

#--------------------------------- 
#---------------------- BLACKBOOST
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

bbooGrid <- expand.grid(
  .mstop = seq(100,300,50),
  .maxdepth = seq(5,20,5) 
)

modFitbboo <-  train(
  festatus ~ .,
  data = trainDat,
  method = "blackboost",
  trControl = bootControl,
  tuneGrid = bbooGrid
)

predbboo <- predict( modFitbboo, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatbboo <- confusionMatrix(testDat$festatus, predbboo); conMatbboo 
conMatbboodf <- as.data.frame(conMatbboo$overall); bbooAcc <- conMatbboodf[1,1]; bbooAcc <- as.character(round(bbooAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(bbooGrid) < 2  )  { resampleHist(modFitbboo) } else  
{ plot(modFitbboo, as.table=T) }

# #Variable Importance
# Impbboo <- varImp( modFitbboo, scale=F)
# plot(Impbboo, top=20)

#Best iteration
modBest <- modFitbboo$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitbboo$times$final[3]
#Samples
samp <- dim(modFitbboo$resample)[1]
numvars <- ncol(trainDat)

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitbboo,
  file=paste("concurso_",numvars,"vars_bboo_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",bbooAcc,"__.RData", sep="")
)


#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *5*, tuneGrid: 20 - Best: 
#Summ: 
#Rest: %.
#ExTm: mins.
#---------
