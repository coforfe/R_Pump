


valScoreMax <- 0.835
for(i in 1:2) {
 print(i) 

  
#--------------------------------- 
#---------------------- SEPARATE TRAINING - TESTING
#--------------------------------- 
library(caret)
set.seed(658754+rnorm(1)*10000)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]

inTrain <- createDataPartition(datSamp$festatus, p=0.99)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]


# #Just temporarily...
# #To load a very interesting iteration : con_35vars_rf_3class_samp100_n_grid5_nobalan__83.47__.RData
#load("con_35vars_rf_3class_samp100_n_grid5_nobalan__84.82__.RData")
load("con_35vars_rf_3class_samp100_n_grid5_nobalan__83.14__.RData")
valScoreMax <- 0.833
for(i in 1:120) {
  print(i)


#--------------------------------- 
#---------------------- RF - RANGER
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
#sedval <- 457856+abs(rnorm(1))*10000
sedval <- (abs(rnorm(1)) * sample(c(100,1000),1))^(sample(c(1:3),1))
#sedval <- 3412467 + rnorm(1) * sample(c(100,1000,10000),1)
#sedval <- 3412467 - i ^ i
if(is.na(sedval)) { next}
set.seed(sedval) 
bootControl <- trainControl(method='oob',number=25, verboseIter=TRUE) 
#bootControl <- trainControl(number=25, verboseIter=TRUE) 

#rfGrid <- expand.grid(.mtry=seq(2,20,1))
rfGrid <- expand.grid(.mtry=5)

modFitrf <-  train(
  festatus ~ .,
  data = trainDat,
  trControl = bootControl,
  tuneGrid = rfGrid,
  method = "ranger",
  num.trees = 500,
  importance = 'impurity',
  verbose = TRUE
  # method = "rf",
  # do.trace = TRUE,
  # ntree =  500
)

predrf <- predict( modFitrf, newdata=testDat[,1:(ncol(testDat)-1)] )
#ConfusionMatrix
conMatrf <- confusionMatrix(testDat$festatus, predrf); conMatrf 
conMatrfdf <- as.data.frame(conMatrf$overall); rfAcc <- conMatrfdf[1,1]; rfAcc <- as.character(round(rfAcc*100,2))
b <- Sys.time();b; b-a   

# if( nrow(rfGrid) < 2  )  { resampleHist(modFitrf) } else  
# { plot(modFitrf, as.table=T) }

#Best iteration
modBest <- modFitrf$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitrf$times$final[3]
#Samples
samp <- dim(modFitrf$resample)[1] ; samp
numvars <- ncol(trainDat); numvars

#Variable Importance
# Imprf <- varImp( modFitrf, scale=F)
# plot(Imprf, top=(ncol(trainDat)-1))

#### CHECK TO CONTINUE
valScore <- conMatrfdf[1,1]
print(c(valScore,sedval))
write(c(valScore, sedval), file="Score.tmp", append=TRUE)
if( valScore < valScoreMax) { next }
valScoreMax <- valScore


#Save trainDat, testDat and Model objects.
modtype <-modFitrf$method
save(
  trainDat, testDat, modFitrf,
  file=paste("con_",numvars,"vars_",modtype,"_3class_samp100_n",samp,"_grid",modBestc,"_nobalan__",rfAcc,"__.RData", sep="")
)



library(stringr)
modFit <- modFitrf
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
sumEr <- round(sqrt(sum(tabEnd[5,]^2)),3) #sqrt(sum(var1^2 + var2^2...))
tabEnd <- rbind(tabEnd, c(sumEr,sumEr,sumEr))
rownames(tabEnd)[6] <- "sumDiff"
tabEnd


timval <- str_replace_all(Sys.time(), " |:", "_")
write.table(datSuboend, file=paste("Res_xxxx_", modtype,"_",numvars,"_samp100_noban_n",samp,"_Acc_", scortmp,"_Err_",sumEr,"_",timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

rm(datSubo, trainTab, testTab, datSubTab, tabEnd, datId, datTmp)

} #for(i in 1:70) {

