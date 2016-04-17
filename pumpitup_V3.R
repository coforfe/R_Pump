#-----------------------------------------------------------------
# Author: Carlos Ortega
# Date: 2015-08-13
# Purpose: Pump-it-up failure prediction.
# Details: http://www.drivendata.org/competitions/7/page/25/
# Pump-Status: functional - functional needs repair - not functional  
# Files: training.csv - trainingStatus.csv - testing.csv - SubmissionFormat.csv 
#-----------------------------------------------------------------
#Caret methods:
#https://cran.rstudio.com/web/packages/caret/news.html
#http://topepo.github.io/caret/modelList.html
#http://topepo.github.io/caret/Bagging.html
#-----------------------------------------------------------------


#----------------------------------------------------
# WORKING DIRECTORY
#----------------------------------------------------
#Working directory
setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp")
#setwd("/Volumes/STORE N GO/R-cosas/2015-08 - PumpItUp")
#load("xgb_glm_knn_nnet_RF_gbm_C50_lasso_fda_plr_Rpart_train_test.RData")


#----------------------------------------------------
# DATA LOADING
#----------------------------------------------------
#Load training  and trainingStatus data 
library(data.table)
datTrain <- fread("training.csv")
datStat <- fread("trainingStatus.csv")
datIn <- datTrain[ , status_group:=datStat$status_group]
datIn <- as.data.frame(datIn)
#all.equal(datTrain$id, datStat$id)

#----------------------------------------------------
# DATA MUNGING - CLEANING
#----------------------------------------------------
#Let's model
#Load caret package
library(caret)
set.seed(1234)
#Clean meaningless columns
colBad <- nearZeroVar(datIn)
#first element of colBad (gps_height in principle is interesting)
datInclean <- datIn[, -colBad[2:length(colBad)]]

#-----------------------------------------------------------
# Added: 2015-08-20 - 05:
# See note "2015-08-20 - 04". New strategy to simplify the dataset.
# To remove:
# id: 1
# amount_tsh: 2 (most of them are 0s)
# date_recorded: 3
# funder: 4 (another more important: installer)
#-----
# problem with installer: 2146 suppliers. Many with just one pump installed.
# It happens that in the test process, it appears suppliers not modeled in the train process.
# Let's model initially without installer
# installer: 6
# Another problem is that too many factors to separate.
#-----
# longitude: 7 / latitude: 8
# wpt_name: 9 / basin: 10  / subvillage: 11 (prefearable to use region)
# As subvillage has 19288 different entries (randomForest, cannot classify more than 32 levels)
# region_code: 13 / district_code: 14 (to use region initially)
# lga: 15 / ward: 16
# public_meeting: 18 (meaning??)
# scheme_management: 19 (is equivalent to management)
# permit: 20 (problems with many NAs difficult to imput them. Not initially)
#-----
# (Analyze failures by extration_type_class)
# (extraction_type_class: 7 classes)
# (extraction_type_group: 13 classes)
# (extraction_type: 18 classes)
# apply(table(datInclean$extraction_type_class, datInclean$status_group), 1, prop.table)*100
# motorpump: 60% bad - other: 83% - wind-power: 56%
# apply(table(datInclean$extraction_type_group, datInclean$status_group), 1, prop.table)*100
# india mark iii: 55% - mono: 61% - other: 83% - other handpump: 52% - other motorpump: 56% - wind-powered: 56%
# apply(table(datInclean$extraction_type, datInclean$status_group), 1, prop.table)*100
# climax: 75% - india mark iii: 54% - ksb: 49% - mono: 61%  - other: 80% - other mkulima..: 100% - other-play-pump: 65% - 
#-----
# extraction_type_group: 23 - extraction_type_class: 24 (extraction_type is more general)
# management_group: 26 (management is more general)
# payment_type: 28 (payment is equivalent)
# water_quality: 29 (quality_group is equivalent)
# quantity_group: 32 (quantity is equivalent)
# source_type: 34 - source_class: 35 (source is more general)
# waterpoint_type_group: 37 (waterpoint_type_group is more general)
# ----------
# To add: howold by using construction_year but there are many values with "0".
# The ones added: feature engineering "fe"...
# fehowold 
fehowold <- ifelse(datInclean$construction_year==0,0, 2015-datInclean$construction_year)
# remove construction_year: 21
# --------
# Status by how old (from 10 to 50 years)
fecuthowold <- cut(fehowold, seq(10,50,10))
# apply(table(fecuthowold, datInclean$status_group), 1, prop.table)*100
# Older than 20 years half are non-functional. Interesting variable.
# To-do: Think about filling rows with values = 0
# ---------
#Status will be "good": functional and "bad": non functional / functional needs repair.
#In that way, status will be more balanced.
#remove also status_group
#toOut <- c(1,2,3,4,7,8,9,10,11,13,14,15,16,18,19,20,21,23,24,26,28,29,32,34,35,37,38)
#toOut <- c(1,2,3,4,7,8,9,10,11,13,14,15,16,18,19,20,21,23,24,26,28,29,32,34,35,37)
toOutxgb <- c(1,2,3,7,8, 18,20)
festatus <- ifelse( (datInclean$status_group=="non functional" | datInclean$status_group=="functional needs repair"), "bad", "good")

#---- Data.frame reduced and enhanced...
#datGod <- datInclean[, -toOut]
datGod <- datInclean[, -toOutxgb]
datGod$fehowold <- fehowold
#datGod$festatus <- as.factor(festatus)
#Leave "status_group" in the original way, but in "festatus" variable.
datGod$festatus <- datGod$status_group
#Remove white spaces in festatus...
library(stringr)
datGod$festatus <- str_replace_all(datGod$festatus," ","_")
datGod$festatus <- as.factor(datGod$festatus)
#Remove "status_group", now status is in "festatus".
#datGod <- datGod[, -c(12)] #With toOut
datGod <- datGod[, -c(31)] #With toOutxgb

#---- 2015-08-26 to include installer but in a treated way...(column 6)
#some installers repeated capital o lower letters...
#everything in lower letters.
insDfori <-  str_to_lower(datGod$installer)
insDf <- as.data.frame(sort(table(insDfori), decreasing=TRUE ))
insDf$Inst <- row.names(insDf)
names(insDf)[1] <- c('Freq')
row.names(insDf) <- NULL
#second installer "empty"..?.
#tenth installer "0"... ?.
dim(insDf)
# [1] 1936    2
# 1936 different installers
#The 50 first installers accounts for the majority of installations...
#plot(insDf$Freq, xlim=c(0,100), ylim=c(0,1000))
#there are some other inconsistences "world", "world bank"...
#"goverment", "goverment central"...
namGod <- insDf$Inst[1:50]
insProc <- ifelse( (insDfori %in% namGod)==TRUE, insDfori, "other")
#now to remove the "" and "0"
insRef <- ifelse( (insProc=="" | insProc=="0"), "unknown", insProc)
#datGod$feinstaller <- insRef #attach to the data.frame - toOut
datGod$feinstaller <- insDfori #just the names tolower.
#remove "installer" and "construction_year" and sort the columns
#datGod <- datGod[, c(1, 3:12,14,13)] #with toOut
datGod <- datGod[, c(1,2, 4:12,14:31,33,32)] #with toOutxgb

#filter one case in "extraction_type" : "other - mkulima/shinyanga"
#just two records and both are "bad"...
#the existence of this creates problems in PLS y PLR models.
#datGod <- datGod[ datGod$extraction_type!="other - mkulima/shinyanga",]

#datGod$fecuthowold <- fecuthowold (not included initially)
#-----------------------------------------------------------------
# datGod with 30 predictors - Just removed the non-meaningul ones.
dim(datGod)
# [1] 59400    31
head(datGod)
save(
      datGod,
      file="datGod.RData"
)
#-----------------------------------------------------------------

# Just to see if it is possible to model, a 25%  sample of the whole set.
set.seed(1)
sizMod <- 0.25 * nrow(datGod)
#sizMod <- 1 * nrow(datGod)
datSamp <- datGod[sample(1:nrow(datGod), sizMod) , ]


#Although working with the training file, I divide between
#training and testing. Later I will check with the testing file.
inTrain <- createDataPartition(datSamp$festatus, p=0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]

#-------------------------
# EQUALLY BALANCED
#-------------------------
#datUp <- upSample(datGod[,1:12], datGod[,13]) #for toOut
datUp <- upSample(datGod[,1:30], datGod[,31]) #for toOutxgb
#names(datUp)[13] <- c('festatus') #for toOut
names(datUp)[31] <- c('festatus') #for toOutxgb
table(datUp$festatus)
# functional functional_needs_repair          non_functional 
# 32259                   32259                   32259 

# Just to see if it is possible to model, a 25%  sample of the whole set.
set.seed(1)
sizModup <- 0.50 * nrow(datUp)
#sizModup <- 1 * nrow(datUp)
datSampUp <- datUp[sample(1:nrow(datUp), sizModup) , ]

inTrainup <- createDataPartition(datSampUp$festatus, p=0.70)[[1]]
trainDatup <- datSampUp[ inTrainup, ]
testDatup <- datSampUp[ -inTrainup, ]


#to run parallel computations code does not change just need these three lines
library(doMC) 
numCor <- parallel::detectCores() - 2 
registerDoMC(cores = numCor)

#-------------------------
# AVAILABLE DATAFRAMES
#-------------------------
# datGod: Has 30 predictors. festatus as the class with the 3 levels. It is not balanced between classes.
# datUp: Has 30 predictors. festatus as the class. with the 3 levels. It has all the classes balanced.


