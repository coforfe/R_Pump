
#--------------------------------- 
#---------------------- C5.0
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(1) 
bootControl <- trainControl(number=5, verboseIter=TRUE) 

c50Grid <- expand.grid(
  .model = "tree",
  .trials = seq(100, 200,100),
  .winnow = "FALSE"
)

modFitc50 <-  train(
 # x = trainDat[, 1:(ncol(testDat)-1)],
  x = trainDat[, 1:10],
  y = trainDat[, ncol(trainDat)],
#   festatus ~ .,
#   data = trainDat,
  method = "C5.0",
  trControl = bootControl,
  tuneGrid = c50Grid
)

predc50 <- predict( modFitc50, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatc50 <- confusionMatrix(testDat$festatus, predc50); conMatc50 
conMatc50df <- as.data.frame(conMatc50$overall); c50Acc <- conMatc50df[1,1]; c50Acc <- as.character(round(c50Acc*100,2))
b <- Sys.time();b; b-a   

if( nrow(c50Grid) < 2  )  { resampleHist(modFitc50) } else  
{ plot(modFitc50, as.table=T) }

# #Variable Importance
# Impc50 <- varImp( modFitc50, scale=F)
# plot(Impc50, top=20)

#Best iteration
modBest <- modFitc50$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitc50$times$final[3]
#Samples
samp <- dim(modFitc50$resample)[1]
numvars <- ncol(trainDat)

#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitc50,
  file=paste("concurso_",numvars,"vars_c50_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",c50Acc,"__.RData", sep="")
)


#Data: 28 variables - 3 classes - 100 sample - not balanced
#Expe: bootstrap: *10*, tuneGrid: (not needed)
#Summ: 
#Rest: %.
#ExTm: mins.
#---------
