####
# Number of Trees (mfinal, numeric) ~ 100 (15)
# Max Tree Depth (maxdepth, numeric)  (5)
# Coefficient Type (coeflearn, character) ~ "Breiman"
####

#--------------------------------- 
#---------------------- ADABAG
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 


adabGrid <- expand.grid(
  .mfinal = 10,
  .maxdepth = seq(5,20,5), 
  .coeflearn = "Freund"
)


modFitadab <-  train(
  festatus ~ .,
  data = trainDat,
  method = "AdaBoost.M1",
  trControl = bootControl,
  tuneGrid = adabGrid
)

predadab <- predict( modFitadab, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatadab <- confusionMatrix(testDat$festatus, predadab); conMatadab 
conMatadabdf <- as.data.frame(conMatadab$overall); adabAcc <- conMatadabdf[1,1]; adabAcc <- as.character(round(adabAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(adabGrid) < 2  )  { resampleHist(modFitadab) } else  
{ plot(modFitadab, as.table=T) }

# #Variable Importance
# Impadab <- varImp( modFitadab, scale=F)
# plot(Impadab, top=20)

#Best iteration
modBest <- modFitadab$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitadab$times$final[3]
#Samples
samp <- dim(modFitadab$resample)[1]
numvars <- ncol(trainDat)

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitadab,
  #file=paste("adab_17var_3class_samp100_n",samp,"_grid",modBestc,"_Downbalan__",adabAcc,"__.RData", sep="")
  file=paste("concurso_",numvars,"vars_bst_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",adabAcc,"__.RData", sep="")
)


#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid: 25 : Best: mfinal=40, maxdepth=5, coefLearn=Freund
#Summ: Gets the maximum at maxdept=15
#Rest: 77.27%.
#ExTm: 34mins.
#---------
#Data: 31 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid: 25 : Best: mfinal=40, maxdepth=5, coefLearn=Freund
#Summ: Only grows with maxtree depth 
#Rest: 72.3%.
#ExTm: 1.17 hours.
