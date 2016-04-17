


#--------------------------------- 
#---------------------- SEPARATE TRAINING - TESTING
#--------------------------------- 
library(caret)
set.seed(658754+rnorm(1)*10000)
#sizMod <- 0.50 * nrow(datIncl)
sizMod <- 1 * nrow(datIncl)
datSamp <- datIncl[sample(1:nrow(datIncl), sizMod) , ]


# shuffle and split the data into three parts
split <- floor(nrow(datSamp)/3)
ensembleData <- datSamp[0:split,]
blenderData <- datSamp[(split+1):(split*2),]
testingData <- datSamp[(split*2+1):nrow(datSamp),]

# set label name and predictors
labelName <- 'festatus'
predictors <- names(ensembleData)[names(ensembleData) != labelName]


#--------------------------------- 
#---------------------- MODELS
#--------------------------------- 
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

a <- Sys.time();a
set.seed(457856+rnorm(1)*10000) 
#bootControl <- trainControl(method='oob',number=25, verboseIter=TRUE) 
bootControl <- trainControl(method='cv',number=5, verboseIter=TRUE, returnResamp='none') 

# train all the ensemble models with ensembleData
model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=bootControl)
model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rpart', trControl=bootControl)
model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=bootControl)
model_ranger <- train(ensembleData[,predictors], ensembleData[,labelName], method='ranger', trControl=bootControl)
model_xgbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='xgbTree', trControl=bootControl)

b <- Sys.time();b-a


# get predictions for each ensemble model for two last data sets
# and add them back to themselves
# Blender
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
blenderData$ranger_PROB <- predict(object=model_ranger, blenderData[,predictors])
blenderData$xgbm_PROB <- predict(object=model_xgbm, blenderData[,predictors])
# Testing
testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])
testingData$ranger_PROB <- predict(object=model_ranger, testingData[,predictors])
testingData$xgbm_PROB <- predict(object=model_xgbm, testingData[,predictors])


# run a final model to blend all the probabilities together
predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='ranger', trControl=bootControl)

# See final prediction and Accuracy of blended ensemble
preds <- predict(object=final_blender_model, testingData[,predictors])
conMatblend <- confusionMatrix(testingData[,labelName], preds); conMatblend 




#--------------------------------- 
#---------------------- FILE TO UPLOAD
#--------------------------------- 
modFit <- final_blender_model
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


