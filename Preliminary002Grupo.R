#-----------------------------------------------------------------
# Author: Collaborative work "madRid" team.
# Date: 2015-09-04
# Purpose: Pump-it-up failure prediction.
# Details: http://www.drivendata.org/competitions/7/page/25/
# Pump-Status: functional - functional needs repair - not functional  
# Files: training.csv - trainingStatus.csv - testing.csv - SubmissionFormat.csv 
# Output: One ".RData" file for each model and group (3 groups considered, each with around 14 algorithms).
# ------------------------------------------------------------------------------
# Caret methods:
# https://cran.rstudio.com/web/packages/caret/news.html
# http://topepo.github.io/caret/modelList.html
# http://topepo.github.io/caret/Bagging.html
# ------------------------------------------------------------------------------

# Variables
# amount_tsh -                Total static head (amount water available to waterpoint)
# date_recorded -             The date the row was entered
# funder -                    Who funded the well
# gps_height -                Altitude of the well
# installer -                 Organization that installed the well
# longitude -                 GPS coordinate
# latitude -                  GPS coordinate
# wpt_name -                  Name of the waterpoint if there is one
# num_private -
# basin -                     Geographic water basin
# subvillage -                Geographic location
# region -                    Geographic location
# region_code -               Geographic location (coded)
# district_code -             Geographic location (coded)
# lga -                       Geographic location
# ward -                      Geographic location
# population -                Population around the well
# public_meeting -            True/False
# recorded_by -               Group entering this row of data
# scheme_management -         Who operates the waterpoint
# scheme_name -               Who operates the waterpoint
# permit -                    If the waterpoint is permitted
# construction_year -         Year the waterpoint was constructed
# extraction_type -           The kind of extraction the waterpoint uses
# extraction_type_group -     The kind of extraction the waterpoint uses
# extraction_type_class -     The kind of extraction the waterpoint uses
# management -                How the waterpoint is managed
# management_group -          How the waterpoint is managed
# payment -                   What the water costs
# payment_type -              What the water costs
# water_quality -             The quality of the water
# quality_group -             The quality of the water
# quantity -                  The quantity of water
# quantity_group -            The quantity of water
# source -                    The source of the water
# source_type -               The source of the water
# source_class -              The source of the water
# waterpoint_type -           The kind of waterpoint
# waterpoint_type_group -     The kind of waterpoint

# Todas "functional":               0.5461
# Todas "non functional":           0.3820
# Todas "functional needs repair":  0.0719

## Some initial work
Sys.setenv(LANGUAGE="en")
set.seed(1234)

# Info session
sessionInfo()

# Show elements working directory
ls()

# Gets you the current working directory
getwd()                    

# Lists all the files present in the current working directory
dir()

# Updates all packages
update.packages()

# ----------------------------------------------------
# WORKING DIRECTORY
#----------------------------------------------------
#Working directory
setwd("INCLUIR DIRECTORIO DE TRABAJO")

#----------------------------------------------------
# DATA LOADING
#----------------------------------------------------

library(readr)
library(corrplot)

# Read competition data files:
train  <- read_csv("./data/training.csv")
status <- read_csv("./data/trainingStatus.csv")
test   <- read_csv("./data/test.csv")

# M <- cor(train[sapply(train, function(x) !is.character(x))])
# corrplot(M, method = "ellipse",order = "hclust")

##Updated
# corrplot(M, method = "number", order = "hclust", type='lower', diag=F, 
#          addCoefasPercent=T)

# First data analysis

# Missing values
length(train[is.na(train)])
length(test[is.na(test)])

dim(train)
dim(test)

names(train)
names(test)

str(train)
str(test)

summary(train)
summary(test)

library(Hmisc)
describe(train)
describe(test)

# Variable changes

# Delete recorded_by (Only one value)
train$recorded_by <- NULL
test$recorded_by  <- NULL

# One set variable
train$set <- 'train'
test$set  <- 'test'

# Status variable
train <- merge(train, status, by.x='id', by.y='id')
names(train)[41] <- 'status'
test$status <- 'NA'

# We create a total dataset 
total <- rbind(train, test)
describe(total)

# 0 values to NA
total$longitude[total$longitude==0] <- NA
total$latitude[total$latitude==0]   <- NA
plot(total$longitude, total$latitude, col=total$region_code)

total$population[total$population==0] <- NA
total$construction_year[total$construction_year==0] <- NA

total$funder[total$funder==""] <- NA
total$status[total$status=="NA"] <- NA
total$installer[total$installer==""] <- NA
total$subvillage[total$subvillage==""] <- NA
total$public_meeting[total$public_meeting==""] <- NA
total$scheme_management[total$scheme_management==""] <- NA
total$scheme_name[total$scheme_name==""] <- NA
total$permit[total$permit==""] <- NA

# Duplicated
total$quantity_group <- NULL

# Unknown values
table(total$management)
table(total$management_group)
table(total$payment)
table(total$payment_type)
table(total$water_quality)
table(total$quality_group)
table(total$quality_group)
table(total$quantity)
table(total$source)
table(total$source_class)

# We'll convert all the characters to factors so we can train a randomForest model on them
toFactorOrdered <- function(data) {
      character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
      for (col in character_cols) {
            data[, col] <- as.factor(data[, col])
            data[, col] <- ordered(data[, col])
            # if needed, conversion to numeric: data[,col] <- as.numeric(data[,col])
      }
      return(data)
}

toFactor <- function(data) {
      character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
      for (col in character_cols) {
            data[, col] <- as.factor(data[, col])
      }
      return(data)
}

toNumeric <- function(data) {
      character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
      for (col in character_cols) {
            data[, col] <- as.factor(data[, col])
            data[, col] <- ordered(data[, col])
            data[, col] <- as.numeric(data[, col])
      }
      return(data)
}

# Train2: Some factors. Train3: All numeric. Train4: Factor ordered

train2 <- toFactor(train)
train3 <- toNumeric(train)
train4 <- toFactorOrdered(train)

test2  <- toFactor(test)
test3  <- toNumeric(test)
test4  <- toFactorOrdered(test)

total2  <- toFactor(total)
total3  <- toNumeric(total)
total4  <- toFactorOrdered(total)

# save(train3, file="./data/train3.RData")
# save(test3, file="./data/test3.RData")
# save(total3, file="./data/total3.RData")

# save(train4, file="./data/train4.RData")
# save(test4, file="./data/test4.RData")
# save(total4, file="./data/total4.RData")

# Kind of variables
classes <- sapply(train, class)
table(classes)

# Numeric (3)
type_numeric <- names(Filter(function(x) x=="numeric", sapply(train, class)))

# Character (30)
type_chracter <- names(Filter(function(x) x=="character", sapply(train, class)))

# Integer (7)
type_integer <- names(Filter(function(x) x=="integer", sapply(train, class)))

# Date (1)
type_date <- names(Filter(function(x) x=="Date", sapply(train, class)))

# False/True

# Levels in train dataset
sapply(train2, levels)

# Levels in test datatset
sapply(test2, levels)

# Different levels
sapply(train2, levels) %in% sapply(test2, levels)
index <- (1:length(test2))[!(sapply(train2, levels) %in% sapply(test2, levels))]
names(train2)[index]
# "funder", "installer", "wpt_name", "subvillage", "ward", "scheme_management", 
# "scheme_name", "extraction_type"  

# M2 <- cor(train3[, c(2, 4:40)])
# corrplot(M2, method = "number", order = "hclust", type='lower', diag = F, 
#          addCoefasPercent=T)

# We change symbol of id (negative to trin), include a status variable and a 
# Set variable
train_id <- (-train$id)
test_id  <- test$id
train_status <- temp$status_group

train5 <- train3
test5  <- test3

train5$id     <- train_id
train5$status <- NULL
train5$status <- train_status
test5$status  <- 0

train5$set    <- 1
test5$set     <- 2

total5 <- rbind(train5, test5)

# save(total5, file="./data/total5.RData")
# save(train5, file="./data/train5.RData")
# save(test5, file="./data/test5.RData")
load("./data/total5.RData")
load("./data/train5.RData")
load("./data/test5.RData")

summary(total5)

table(total5$T1_V1, useNA = 'ifany')
table(train5$T1_V1, useNA = 'ifany')

library(corrplot)
M3 <- cor(total5[, c(2, 5:40)])
corrplot(M3, method = "number", order = "hclust", type='lower', diag=F, 
         addCoefasPercent=T)

for (i in 2:42) {
      print(paste('Variable', names(train5)[i]))
      print(table(train5[, i]))
      plot(table(train5[, i]), main = names(train5)[i])
}

for (i in 2:42) {
      print(paste('Variable', names(total5)[i]))
      print(table(total5[, i]))
      plot(table(total5[, i]), main = names(total5)[i])
}

# Unique values
apply(total5[, 2:35], 2, unique)
apply(train[, 2:34], 2, unique)

################################################################################



################################################################################
## Models

library(h2o)
localH2O <- h2o.init(nthread=-1, max_mem_size="16g")

RMSE <- function(pre, real)
{
      return(sqrt(mean((pre-real)*(pre-real))))
}

# Other h2o Deep Lerning

load("./data/total.RData")

index_train <- (1:dim(total)[1])[total$set=='train']
index_test  <- (1:dim(total)[1])[total$set=='test']

h2o_train     <- as.h2o(localH2O, total[index_train, ])
h2o_test      <- as.h2o(localH2O, total[index_test, ])

total_train <- total[index_train, ]
total_test  <- total[index_test, ]

# save(total5_train, file="./data/total5_train.RData")
# save(total5_test, file="./data/total5_test.RData")

library(caret)

load("./data/total5.RData")

index_train <- (1:dim(total5)[1])[total5$set==1]
index_test  <- (1:dim(total5)[1])[total5$set==2]

train6 <- total5[index_train, ]
test6  <- total5[index_test, ]

inTrain6  <- createDataPartition(y=train5$status, p=0.7, list=FALSE)
training6 <- train6[inTrain6,] 
testing6  <- train6[-inTrain6,]

# save(test6, file="./data/test6.RData")
# save(testing6, file="./data/testing6.RData")
# save(train6, file="./data/train6.RData")
# save(training6, file="./data/training6.RData")

################################################################################
library(h2o)
localH2O <- h2o.init(nthread=4, max_mem_size="12g")

RMSE <- function(pre, real)
{
      return(sqrt(mean((pre-real)*(pre-real))))
}

# Other h2o Deep Lerning

load("./data/total.RData")

total$status <- as.factor(total$status)

index_train <- (1:dim(total)[1])[total$set=='train']
index_test  <- (1:dim(total)[1])[total$set=='test']

h2o_train   <- as.h2o(localH2O, total[index_train, ])
h2o_test    <- as.h2o(localH2O, total[index_test, 1:39])

variables_001 <- c(2, 4:38)

### H2O Deep learning ###
variables_001 <- c(2, 4:38)
mod_fit001 <- h2o.gbm(x = variables_001, y = 40, 
                      training_frame = h2o_train,
                      # distribution="AUTO",
                      # nfolds = 1,
                      seed = 1234,
                      ntrees = 1000,
                      max_depth = 5,
                      min_rows = 10,
                      nbins = 20,
                      learn_rate = 0.01)
mod_fit001

test001 <- as.data.frame(h2o.predict(mod_fit001, h2o_train))$predict
test001
plot(test001)

pred001 <- as.data.frame(h2o.predict(mod_fit001, h2o_test))$predict
plot(pred001, main='Pred001')

save(mod_fit001, file="./data/mod_fit001.RData")
save(test001, file="./data/test001.RData")
write.csv(data.frame(id=total$id[index_test], status=pred001),
          "./pred/pred001.csv", row.names=F, quote=FALSE)

# pred001 <- as.data.frame(h2o.predict(mod_fit001, h2o_test))$predict

mod_fit002 <- h2o.deeplearning(x = variables_001, y = 40,
                               training_frame = h2o_train, 
                               # classification=FALSE, 
                               activation = "Tanh", 
                               hidden = c(512, 512, 512),
                               input_dropout_ratio = 0.2,
                               # validation=test_hex,
                               epochs = 128, 
                               variable_importances = T)
mod_fit002

test002 <- as.data.frame(h2o.predict(mod_fit002, h2o_train))$predict
test002
plot(test002, main='Test002')

pred002 <- as.data.frame(h2o.predict(mod_fit002, h2o_test))$predict
plot(pred002, main='Pred002')

save(mod_fit002, file="./data/mod_fit002.RData")
save(test002, file="./data/test002.RData")
write.csv(data.frame(id=total$id[index_test], status=pred002),
          "./pred/pred002.csv", row.names=F, quote=FALSE)

mod_fit003 <- h2o.deeplearning(x = variables_001, y = 40,
                               training_frame = h2o_train, 
                               # classification=FALSE, 
                               activation = "Tanh", 
                               hidden = c(1024, 1024, 1024),
                               input_dropout_ratio = 0.2,
                               # validation=test_hex,
                               epochs = 128, 
                               variable_importances = T)
mod_fit003

test003 <- as.data.frame(h2o.predict(mod_fit003, h2o_train))$predict
test003
plot(test003, main='Test003')

pred003 <- as.data.frame(h2o.predict(mod_fit003, h2o_test))$predict
plot(pred003, main='Pred003')

save(mod_fit003, file="./data/mod_fit003.RData")
save(test003, file="./data/test003.RData")
write.csv(data.frame(id=total$id[index_test], status=pred003),
          "./pred/pred003.csv", row.names=F, quote=FALSE)

mod_fit004 <- h2o.randomForest(x = variables_001, y = 40,
                               training_frame = h2o_train,
                               seed = 1967,
                               ntrees = 5000, 
                               max_depth = 25,
                               min_rows = 1,
                               nbins = 20)
mod_fit004

test004 <- as.data.frame(h2o.predict(mod_fit004, h2o_train))$predict
table(test004)
plot(test004, main='Test004')

pred004 <- as.data.frame(h2o.predict(mod_fit004, h2o_test))$predict
plot(pred004, main='Pred004')

save(mod_fit004, file="./data/mod_fit004.RData")
save(test004, file="./data/test004.RData")
write.csv(data.frame(id=total$id[index_test], status=pred004),
          "./pred/pred004.csv", row.names=F, quote=FALSE)


mod_fit005 <- h2o.randomForest(x = variables_001, y = 40,
                               training_frame = h2o_train,
                               seed = 1967,
                               ntrees = 7000, 
                               max_depth = 25,
                               min_rows = 1,
                               nbins = 20)
mod_fit005

test005 <- as.data.frame(h2o.predict(mod_fit005, h2o_train))$predict
table(test005)
plot(test005, main='Test005')

pred005 <- as.data.frame(h2o.predict(mod_fit005, h2o_test))$predict
plot(pred005, main='Pred005')

save(mod_fit005, file="./data/mod_fit005.RData")
save(test005, file="./data/test005.RData")
write.csv(data.frame(id=total$id[index_test], status=pred005),
          "./pred/pred005.csv", row.names=F, quote=FALSE)

# Submission name: h20_randomforest_all_tree7000_depth25_row1_bin_20_id005.csv


mod_fit006 <- h2o.randomForest(x = variables_001, y = 40,
                               training_frame = h2o_train,
                               seed = 1967,
                               ntrees = 2000, 
                               max_depth = 50,
                               min_rows = 1,
                               nbins = 35)
mod_fit006

test006 <- as.data.frame(h2o.predict(mod_fit006, h2o_train))$predict
table(test006)
plot(test006, main='Test006')

pred006 <- as.data.frame(h2o.predict(mod_fit006, h2o_test))$predict
plot(pred006, main='Pred006')

save(mod_fit006, file="./data/mod_fit006.RData")
save(test006, file="./data/test006.RData")
write.csv(data.frame(id=total$id[index_test], status=pred006),
          "./pred/pred006.csv", row.names=F, quote=FALSE)

