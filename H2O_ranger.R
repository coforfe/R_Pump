
library(caret)
seed(998)

iris$isSetosa = ifelse(iris$Species == "setosa", 1.0, 0.0)
iris$isSetosa = as.factor(iris$isSetosa)
mydata = iris[,-5]
#############################################################
inTraining <- createDataPartition(mydata$isSetosa, p = 0.75, list = F)
t <- iris[inTraining, ]
h <- iris[- inTraining, ]

rfControl <- trainControl(method = "boot", number = 1, repeats = 1)
rfControl2 <- trainControl(method = "none", number = 1, repeats = 1)
rfControl3 <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
rfGrid <- expand.grid(mtry = c(2))

system.time( train( isSetosa ~ ., data = t, method = "rf", ntree = 100, 
                    trControl = rfControl, tuneGrid = rfGrid))

system.time( train( isSetosa ~ ., data = t, method = "rf", ntree = 100, 
                    trControl = rfControl2, tuneGrid = rfGrid))

system.time( train( isSetosa ~ ., data = t, method = "rf", ntree = 100, 
                    trControl = rfControl3, tuneGrid = rfGrid))

system.time( randomForest(isSetosa ~ ., data = t, ntree = 100, mtry = 2))

#-----------------------------------------------------------

irisbig <- rbind(iris, iris)
for(i in 1:1000){
  irisbig <- rbind(irisbig, iris)
}

rfGrid <- expand.grid(mtry = c(2))
rfControl2 <- trainControl(method = "none", number = 1, repeats = 1)

system.time( train( isSetosa ~ ., data = irisbig, method = "rf", ntree = 100, 
                    trControl = rfControl2, tuneGrid = rfGrid))

system.time( randomForest(isSetosa ~ ., data = irisbig, ntree = 100, mtry = 2))

