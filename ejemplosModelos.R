
setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp")

library(caret)
data(BloodBrain)
set.seed(1)

library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

tmp <- createDataPartition(logBBB,
                             p = .8,
                             times = 100)

a <- Sys.time();a 
rpartFit <- train(bbbDescr, logBBB,
                    "rpart", 
                    tuneLength = 16,
                    trControl = trainControl(
                      method = "LGOCV", index = tmp))
b <- Sys.time();b; b-a 

a <- Sys.time();a 
ctreeFit <- train(bbbDescr, logBBB,
                    "ctree", 
                    trControl = trainControl(
                      method = "LGOCV", index = tmp))
b <- Sys.time();b; b-a 

#   earthFit <- train(bbbDescr, logBBB,
#                     "earth",
#                     tuneLength = 20,
#                     trControl = trainControl(
#                       method = "LGOCV", index = tmp))

  
a <- Sys.time();a 
dnnFit <- train(bbbDescr, logBBB,
                  "dnn",
                  tuneLength = 10,
                  trControl = trainControl(
                    method = "LGOCV", index = tmp))
plot(dnnFit)
b <- Sys.time();b; b-a 
# dnn -> 3 hidden layers. tuneLength defines the number of neurons in each layer.
# tuneLength=5 -> 2.9 mins.
# tuneLength=10 -> 24.23 mins (10x)
  
#--------------
a <- Sys.time();a 
cvCtrl <- trainControl(
  method = "repeatedcv", repeats = 5, 
  #summaryFunction = twoClassSummary,
  #classProbs = TRUE, 
  verboseIter = TRUE
)
cvCtrlB <- trainControl(
  method = "LGOCV", index = tmp
)
xgbLGrid = expand.grid( 
  .nrounds=seq(1, 20),
  .alpha = 1,
  .lambda = seq(0.001, 0.1, length=5)
)
xgbLFit <-  train(
                  bbbDescr, logBBB,
                  "xgbLinear",
                  tuneLength = 20,
                  tuneGrid = xgbLGrid,
                  trControl =  cvCtrl
                 )
plot(xgbLFit)
b <- Sys.time();b; b-a 
#21 secs.
#--------------

a <- Sys.time();a 
xgbTreeFit <-  train(bbbDescr, logBBB,
                    "xgbTree",
                    tuneLength = 20,
                    trControl = trainControl(
                      method = "LGOCV", index = tmp))
 b <- Sys.time();b; b-a  
 #9 mins.
  
a <- Sys.time();a 
elmFit <-  train(bbbDescr, logBBB,
                       "elm",
                       tuneLength = 20,
                       trControl = trainControl(
                         method = "LGOCV", index = tmp))
b <- Sys.time();b; b-a   
# 32 secs.
  
 a <- Sys.time();a 
data(shuttle, package="MASS")
nbFit <-  train(x=myshuttle[,1:16], y=myshuttle[,17],
                  "nb",
                  tuneLength = 10,
                  trControl = trainControl(
                    method = "LGOCV", index = tmp))
plot(nbFit)
b <- Sys.time();b; b-a   
#nb does not work with data all numeric
## wrong type for regression. data.frame everything numeric...
#For all character data (shuttle) -> 5 secs. Accuracy=0.5. tuneLength=5.
#For myshuttle (artificial numeric + character with character class) -> 17 secs.
#For tuneLength=10 -> 19 secs.

a <- Sys.time();a 
parRFFit <-  train(bbbDescr, logBBB,
                 "parRF",
                 tuneLength = 20,
                 trControl = trainControl(
                   method = "LGOCV", index = tmp))
b <- Sys.time();b; b-a   
# 12.41 mins.

a <- Sys.time();a 
gcvEFit <-  train(bbbDescr, logBBB,
                   "gcvEarth",
                   tuneLength = 20,
                   trControl = trainControl(
                     method = "LGOCV", index = tmp))
b <- Sys.time();b; b-a   
# 8 secs. 

#------------------------------
a <- Sys.time();a 
data(shuttle, package="MASS")
library(randomNames)
myshuttle <- cbind.data.frame( as.data.frame(matrix(rnorm(256*10), nrow=256)), shuttle)
names(myshuttle)[1:10] <- randomNames(10, which.names="first")
tmpval <- data.frame()
for(i in 1:200) {
 tmpval <- rbind.data.frame(tmpval, myshuttle) 
}
myshuttle <- tmpval; rm(tmpval)
myshuttle[,1] <- randomNames(nrow(myshuttle), which.names="first")

myshuttle <- datSamp
gcvEFit <-  train(
                  x = myshuttle[,1:11],
                  y = myshuttle[,12],
                  "rFerns",
                  tuneLength = 2,
                  trControl = trainControl(
                    method = "LGOCV", index = tmp)
                 )
b <- Sys.time();b; b-a   
plot(gcvEFit)
# 6.87 min -> tuneLength=10.
# 2.46 min -> tuneLength=3
# 5.3 min -> tuneLength=2, 10 more variables. 256 x 17
# 7.16 min - tuneLength=2 .bigger data.frame = 51200 x 17.
#  min - tuneLength=2 .bigger data.frame = 51200 x 17. - First column randomNames...
## Error en "train"... too many levels in first column...



  
  #or load pre-calculated results using:
  #load(url("http://caret.r-forge.r-project.org/exampleModels.RData"))

  resamps <- resamples(
                       list(CART = rpartFit,
                            CondInfTree = ctreeFit,
                            #MARS = earthFit,
                            #AdaBag = adaFit,
                            #Dnn = dnnFit,
                            XGBL = xgbLFit,
                            XBGTree = xgbTreeFit,
                            ELM = elmFit,
                            PARRF = parRFFit,
                            GCVEARTH = gcvEFit
                            )
                       )

  resamps
  summary(resamps)
  modelCor(resamps)
  
  ## The code for train() does not change:
  library(mlbench)
  data(BostonHousing)
  
  set.seed(1)
  a <- Sys.time();a
  usingMC <-  train(medv ~ .,
                    data = BostonHousing, 
                    method = "glmboost")
  b <- Sys.time();b; b-a
  
  set.seed(1)
  trainDat <- BostonHousing[, 1:13]
  classDat <- BostonHousing[, 14]
  a <- Sys.time();a
  usingMC <-  train(
                    trainDat, classDat,
                    method = "glmboost"
                    )
  b <- Sys.time();b; b-a 
  
  
  data(iris)
  m <- NaiveBayes(Species ~ ., data = iris)
  respred <- predict(m) 
  table(Prediction=respred$class, Original=iris$Species)
  
  
  
  
 #---------------------------------------------------------------- 
 #---------------------------------------------------------------- 
 #---------------------------------------------------------------- 
 #---------------------------------------------------------------- 
  
  
 #--------------------------------- 
 #---------------------- GBM
 #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
#   tmp <- createDataPartition(trainDat[,12],
#                              p = .8,
#                              times = 50)
bootControl <- trainControl(number=10) 
# gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
#                         .n.trees = (1:10)*25, .shrinkage = .1,
#                        .n.minobsinnode = 5 )
gbmGrid <- expand.grid(.interaction.depth = 10,
                        .n.trees = 250, .shrinkage = .1,
                       .n.minobsinnode = 5 )
  
#Transform character columns in factor
for( i in 1:ncol(trainDat)) {
  cltmp <- class(trainDat[, i])
  if(cltmp == "character") {
    trainDat[, i] <- as.factor(trainDat[,i])
  } else next
}
for( i in 1:ncol(testDat)) {
  cltmp <- class(testDat[, i])
  if(cltmp == "character") {
    testDat[, i] <- as.factor(testDat[,i])
  } else next
}

  modFitgbm <-  train(
    x = trainDat[,1:11],
    y = trainDat[,12],
    method = "gbm",
    # tuneLength = 3,
#     trControl = trainControl(
#       method = "LGOCV", index = tmp)
    trControl = bootControl,
    verbose = TRUE, 
    bag.fraction = 0.5, 
    tuneGrid = gbmGrid
  )
  
  predgbm <- predict(modFitgbm, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatgbm <- confusionMatrix(testDat$festatus, predgbm) 
  b <- Sys.time();b; b-a   
  plot(modFitgbm)
  
  save(
    trainDat, testDat, modFitgbm,
    file="gbm_3class_050_nobalan.RData"
  )
 
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: 75%.
  #Execution time: 4 min.
  #---------
  #Result: 11 variables - 3 classes - 50 sample - not balanced
  #Result: 76.6%.
  #Execution time: 4.49 min.
  
  
  #--------------------------------- 
  #---------------------- XGB
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=200) 
 
#   xgbGrid <- expand.grid(
#                          .eta = seq(0.05, 0.1, 0.01),
#                          .max_depth = 10:15,
#                          .nrounds = seq(200, 400,50)
#                          )
  
  xgbGrid <- expand.grid(
    .eta = 0.07,
    .max_depth = 10,
    .nrounds = 200
  ) 
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitxgb <-  train(
    festatus ~ .,
    data = trainDat,
    #x = trainDat[,1:11],
    #y = trainDat[,12],
    method = "xgbTree",
    trControl = bootControl,
    verbose = 1,
    num_class = 3,
    tuneGrid = xgbGrid
  )
  
  predxgb <- predict(modFitxgb, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatxgb <- confusionMatrix(testDat$festatus, predxgb) 
  b <- Sys.time();b; b-a   
  if( nrow(xgbGrid) < 2  )  { resampleHist(modFitxgb) } else  
    { plot(modFitxgb, as.table=T) }
  
  save(
    trainDat, testDat, modFitxgb,
    file="xgb_11var_3class_samp100_n200_grid1_nobalan.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: Just one value for each parameter in tuneGrid.
  #Result: 74%.
  #Execution time:  30 secs.
  #It works only in a FORMULA WAY!!
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: grid of 90 different values. Iterations=1
  #Result: 76%.
  #Execution time: 1.72mins.  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: grid of 90 different values. Iterations=10
  #Result: 75.84%.
  #Execution time: 16.82 mins.  
  #---------
  #Result: 11 variables - 3 classes - 50 sample - not balanced
  #Result: grid of 80 different new values. Iterations=5
  #Result: Better with shrinkage: 0.1, max_tree: 10, nrounds=200
  #Result: 77%.
  #Execution time: 38.82 mins.  
  #---------
  #Result: 11 variables - 3 classes - 50 sample - not balanced
  #Result: grid of 80 different new values. Iterations=5
  #Result: Better with shrinkage: 0.05 (just one value -mistake-), max_tree: 13, nrounds=200
  #Result: 77%.
  #Execution time: 31 mins.  
  #---------
  #Result: 11 variables - 3 classes - 50 sample - not balanced
  #Result: grid of 180 different new values. Iterations=5
  #Result: Better with shrinkage: nrounds=200, shrinkage=0.07, max_tree:10:13.
  #Result: 77.51%.
  #Execution time:  3 hrs 21 min.   
  #---------
  #Result: 11 variables - 3 classes - 100 sample - not balanced
  #Result: tuneGrid: 1 value. Iterations=100
  #Result: rounds=200, shrinkage=0.07, max_tree:10.
  #Result: 78.61%.
  #Execution time: 1 hour. 
  #---------
  #Result: 11 variables - 3 classes - 100 sample - not balanced
  #Result: tuneGrid: 1 value. Iterations=200
  #Result: rounds=200, shrinkage=0.07, max_tree:10.
  #Result: 78.61%.
  #Execution time:  2.12 hours. 
  
  
  #--------------------------------- 
  #---------------------- RF
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=200) 
  
#   rfGrid <- expand.grid(
#     .mtry = seq(2, 3, 1)
#   )
 rfGrid <- expand.grid(.mtry=2)
  
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitrf <-  train(
#     festatus ~ .,
#     data = trainDat,
    x = trainDat[,1:11],
    y = trainDat[,12],
    method = "rf",
    trControl = bootControl,
    tuneGrid = rfGrid,
    do.trace = TRUE,
    ntree =  100
  )
  
  predrf <- predict(modFitrf, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatrf <- confusionMatrix(testDat$festatus, predrf) 
  b <- Sys.time();b; b-a   
  
  if( nrow(rfGrid) < 2  )  { resampleHist(modFitrf) } else  { plot(modFitrf, as.table=T) }
  
  save(
    trainDat, testDat, modFitrf,
    file="rf_11var_3class_samp100_boot200_nobalan.grid1.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 050 sample - not balanced - 1 samples
  #Result: tuneGrid: mtry: 3
  #Result: 69.92%.
  #Execution time:  4 mins.
  #---------
  #Result: 11 variables - 3 classes - 050 sample - not balanced - 5 samples
  #Result: tuneGrid: mtry: 2:7 - Best: mtry=2
  #Result: 76.8%.
  #Execution time:  2 hrs. 28 min. 
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced - 4 samples
  #Result: tuneGrid: mtry: 2:3 - Best: mtry=2
  #Result: 75.95%.
  #Execution time:  13min. 
  #---------
  #Result: 11 variables - 3 classes - 100 sample - not balanced - 4 samples
  #Result: bootstrap: 50, tuneGrid: mtry=2 + ntree=100 (not 500 default)
  #Result: 78.92%. (!!!)
  #Execution time: 33 min. 
  #---------
  #Result: 11 variables - 3 classes - 100 sample - not balanced - 4 samples
  #Result: bootstrap: 200, tuneGrid: mtry=2 + ntree=100 (not 500 default)
  #Result: 78.94%.
  #Execution time: 2 hrs 17min. 
  
  
  
  #--------------------------------- 
  #---------------------- Oblique orfsvm
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=4) 
  
  orfsvmGrid <- expand.grid(
    .mtry = seq(2, 10, 1)
  )
  # orfsvmGrid <- expand.grid( .mtry=2)
  
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitorfsvm <-  train(
    #     festatus ~ .,
    #     data = trainDat,
    x = trainDat[,1:11],
    y = trainDat[,12],
    method = "ORFsvm",
    trControl = bootControl,
    tuneGrid = orfsvmGrid,
    do.trace = TRUE
  )
  
  predorfsvm <- predict(modFitorfsvm, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatorfsvm <- confusionMatrix(testDat$festatus, predorfsvm) 
  b <- Sys.time();b; b-a   
  plot(modFitorfsvm, as.table=T)
  
  save(
    trainDat, testDat, modFitorfsvm,
    file="orfsvm_11var_3class_025_nobalan.grid9val.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced - 4 samples
  #Result: tuneGrid: mtry: 2:3 - Best: 2=3
  #Result:53.77%. (MALO!!)
  #Execution time:  2 min.  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced - 4 samples
  #Result: tuneGrid: mtry: 2:10 - Best: 
  #Result:53.77%. (MALO!!)
  #Execution  time: 8 mins.    
  
  #--------------------------------- 
  #---------------------- Oblique orfrid
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=4) 
  
  orfridGrid <- expand.grid(
    .mtry = seq(2, 10, 1)
  )
  # orfridGrid <- expand.grid( .mtry=2)
  
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitorfrid <-  train(
    #     festatus ~ .,
    #     data = trainDat,
    x = trainDat[,1:11],
    y = trainDat[,12],
    method = "ORFridge",
    trControl = bootControl,
    tuneGrid = orfridGrid,
    do.trace = TRUE
  )
  
  predorfrid <- predict(modFitorfrid, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatorfrid <- confusionMatrix(testDat$festatus, predorfrid) 
  b <- Sys.time();b; b-a   
  plot(modFitorfrid, as.table=T)
  
  save(
    trainDat, testDat, modFitorfrid,
    file="orfrid_11var_3class_025_nobalan.grid9val.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced - 4 samples
  #Result: tuneGrid: mtry: 2:3 - Best: 2=3
  #Result:53.77%. (MALO!!)
  #Execution time:  2 min.  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced - 4 samples
  #Result: tuneGrid: mtry: 2:10 - Best: 
  #Result:53.77%. (MALO!!)
  #Execution  time: 8 mins.    
  
  #--------------------------------- 
  #---------------------- Oblique orfrid
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=4) 
  
  orfridGrid <- expand.grid(
    .mtry = seq(2, 10, 1)
  )
  # orfridGrid <- expand.grid( .mtry=2)
  
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitorfrid <-  train(
    #     festatus ~ .,
    #     data = trainDat,
    x = trainDat[,1:11],
    y = trainDat[,12],
    method = "ORFridge",
    trControl = bootControl,
    tuneGrid = orfridGrid,
    do.trace = TRUE
  )
  
  predorfrid <- predict(modFitorfrid, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatorfrid <- confusionMatrix(testDat$festatus, predorfrid) 
  b <- Sys.time();b; b-a   
  plot(modFitorfrid, as.table=T)
  
  save(
    trainDat, testDat, modFitorfrid,
    file="orfrid_11var_3class_025_nobalan.grid9val.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced - 4 samples
  #Result: tuneGrid: mtry: 2:3 - Best: 2=3
  #Result:   53.77%.
  #Execution time:  8 min.  
  #---------
  
  
  #--------------------------------- 
  #---------------------- XGB Linear
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=200) 
  
#   xgbGridL <- expand.grid(
#     .nrounds = seq(100,200,50),
#     .lambda = seq(0.2,0.3,0.1),
#     .alpha = seq(0.2, 0.3, 0.1)
#   )

  xgbGridL <- expand.grid(
    .nrounds = 100,
    .lambda = 0.3,
    .alpha = 0.2
  )
  
    
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitxgbL <-  train(
    festatus ~ .,
    data = trainDat,
    #x = trainDat[,1:11],
    #y = trainDat[,12],
    method = "xgbLinear",
    trControl = bootControl,
    verbose = 1,
    num_class = 3,
    tuneGrid = xgbGridL
  )
  
  predxgbL <- predict(modFitxgbL, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatxgbL <- confusionMatrix(testDat$festatus, predxgbL) 
  b <- Sys.time();b; b-a   
  
  if( nrow(xgbGridL) < 2  )  { resampleHist(modFitxgbL) } else 
    { plot(modFixgbL, as.table=T) }
  
  save(
    trainDat, testDat, modFitxgbL,
    file="xgbL_11var_3class_100_grid1_n200_nobalan.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: 5 samples - tuneGrid: 27 vars . Best: nrounds 200 - L1:0.3, L2:0.20 
  #Result: 76.38%.
  #Execution time: 15min.
  #---------
  #Result: 11 variables - 3 classes - 50 sample - not balanced
  #Result: 10 samples - tuneGrid: 12 vars . Best: nrounds=100, lambda=0.3, alpha=0.2 
  #Result: 77.06%.
  #Execution time: 16 min.
  #---------
  #Result: 11 variables - 3 classes - *75* sample - not balanced
  #Result: *100* samples - tuneGrid: fixed . Best: nrounds=100, lambda=0.3, alpha=0.2 
  #Result: 77.9%.
  #Execution time: 13 min.
  #---------
  #Result: 11 variables - 3 classes - *100* sample - not balanced
  #Result: Boot:*100* - tuneGrid: fixed . Best: nrounds=100, lambda=0.3, alpha=0.2 
  #Result: 77.67%.
  #Execution time: 17 min.
  #---------
  #Result: 11 variables - 3 classes - *100* sample - not balanced
  #Result: Boot:200 - tuneGrid: fixed . Best: nrounds=100, lambda=0.3, alpha=0.2 
  #Result: 77.67%.
  #Execution time: 34 min.
  
   
  
  #--------------------------------- 
  #---------------------- SVM Radial
  #--------------------------------- 
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=10) 
  
#   svmGrid = expand.grid( 
#     .C = c(1, 5, 10, 50, 100),
#     .sigma = c(0.001, 0.005, 0.01, 0.05)
#   )
#   
#   svmGrid = expand.grid( 
#     .C = c(100, 500),
#     .sigma = c(0.0001,0.001)
#   ) 
 
  svmGrid = expand.grid( 
    .C = c(100),
    .sigma = c(0.001)
  ) 
  
   
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitsvm <-  train(
    festatus ~ .,
    data = trainDat,
    #x = trainDat[,1:11],
    #y = trainDat[,12],
    method = "svmRadial",
    trControl = bootControl,
    verbose = TRUE,
    tuneGrid = svmGrid
  )
  
  predsvm <- predict(modFitsvm, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatsvm <- confusionMatrix(testDat$festatus, predsvm) 
  b <- Sys.time();b; b-a   
  
  if( nrow(svmGrid) < 2  )  { resampleHist(modFitsvm) } else 
    { plot(modFisvm, as.table=T) }
  
  save(
    trainDat, testDat, modFitsvm,
    file="svm_11var_3class_samp050_n10_grid1_nobalan.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: 3 samples - tuneGrid: 4 vars. Best: C:100 - sig:0.001 
  #Result: 74.32%.
  #Execution time: 9 min. 
  #---------
  #Result: 11 variables - 3 classes - 025 sample - not balanced
  #Result: 4 samples - tuneGrid: 4 vars. Best: C:100 - sig:0.001 
  #Result: 74.32%.
  #Execution time: 11 min. 
  #---------
  #Result: 11 variables - 3 classes - 50 sample - not balanced
  #Result: 10 samples - tuneGrid: 1 vars. C:100 - sig:0.001 
  #Result: 75.22%.
  #Execution time:  37 min.   
  
  
  #--------------------------------- 
  #---------------------- C5.0 Tree
  #--------------------------------- 
  library(doMC) 
  numCor <- parallel::detectCores() - 2 
  registerDoMC(cores = numCor)
  
  a <- Sys.time();a
  set.seed(1) 
  bootControl <- trainControl(number=200) 
  
  
  #Transform character columns in factor
  for( i in 1:ncol(trainDat)) {
    cltmp <- class(trainDat[, i])
    if(cltmp == "character") {
      trainDat[, i] <- as.factor(trainDat[,i])
      testDat[, i] <- as.factor(testDat[,i])
    } else next
  }
  
  modFitc50 <-  train(
#     festatus ~ .,
#     data = trainDat,
    x = trainDat[,1:11],
    y = trainDat[,12],
    method = "C5.0Tree",
    trControl = bootControl,
    verbose = TRUE
    #tuneGrid = c50Grid
  )
  
  predc50 <- predict(modFitc50, newdata=testDat[,1:11])
  #ConfusionMatrix
  conMatc50 <- confusionMatrix(testDat$festatus, predc50) 
  b <- Sys.time();b; b-a   
  
  resampleHist(modFitc50) 
#   if( nrow(c50Grid) < 2  )  { resampleHist(modFitc50) } else 
#   { plot(modFic50, as.table=T) }
  
  save(
    trainDat, testDat, modFitc50,
    file="c50_11var_3class_samp100_n200_grid1_nobalan.RData"
  )
  
  #---------
  #Result: 11 variables - 3 classes - 100 sample - not balanced
  #Result: 100 Boot - tuneGrid: no.
  #Result: 77.43%.
  #Execution time:  6.34min. 
  #---------
  #---------
  #Result: 11 variables - 3 classes - 100 sample - not balanced
  #Result: 200 Boot - tuneGrid: no.
  #Result: 77.43%.
  #Execution time:  13 min. 
  #---------  
  