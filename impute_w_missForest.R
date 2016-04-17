

#----------------------------------------------------
# WORKING DIRECTORY
#----------------------------------------------------
#Working directory
setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp")


#----------------------------------------------------
# TRAIN AND TEST SETS
#----------------------------------------------------
library(data.table)
library(stringr)
datIncl <- fread("myTrain.csv")
datIncl <- as.data.frame(datIncl)
names(datIncl)[31] <- c('festatus')
datIncl <- datIncl[, c(1:30, 32:33,31)] #reorder data.frame
datIncl$festatus <- as.factor(datIncl$festatus)
#datIncl <- datIncl[, -c(11,13)] #remove permit and  public_meeting.
datIncl$festatus <- str_replace_all(datIncl$festatus, " ", "_")
datIncl <- datIncl[, -c(3,5)] #remove funder, installer, if not it does not work... It cannot be all equal.

# Loading to use some new datIn columns (num_private)
library(data.table)
datTrain <- fread("training.csv")
datStat <- fread("trainingStatus.csv")
datIn <- datTrain[ , status_group:=datStat$status_group]
datIn <- as.data.frame(datIn)
rm(datTrain, datStat)

#Remove num_private
#datIncl$num_private <- datIn$num_private
#datIncl <- datIncl[,c(1:30,32,31)] #reorder

#To transform in factors the character columns. It's very convenient.
for( i in 1:ncol(datIncl)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "character") {
    datIncl[, i] <- as.factor(datIncl[,i])
  } else next
}



#--------------------------------- 
#--------------------- Feature Engineering
#--------------------------------- 

# To fill long/lat, gps_height... first to include region_code/district_code
# they will act as a reference (they have no ceros) for the rest of the columns to impute.
#Add region_code and district_code both are numeric
datIncl$region_code <- datIn$region_code
datIncl$district_code <- datIn$district_code
#Also as a reference I include region as basin as a numeric to help the imputation process.
datIncl$region <- as.numeric(datIncl$region)
datIncl$basin <- as.numeric(datIncl$basin)

#---------- Fill ceros in longitude and gps_height
# longitutde has 1812 ceros.sum(datIn$longitude==0)
# latitude has no ceros...sum(datIn$latitude==0)
# gps_height hast 20438 ceros.sum(datIn$gps_height==0)
# Lets fill longitude and gps_height ceros using latitude.
#library(imputeR)
lonlatgps <- datIncl[, c('latitude', 'longitude','gps_height', 'amount_tsh','population','region_code', 'district_code', 'basin', 'region')]
# To make better prediction indlue "ward" and "lga"  and "subvillage" from datIn although they are not included in datIncl
lonlatgps$ward <- as.numeric(as.factor(datIn$ward))
lonlatgps$lga <- as.numeric(as.factor(datIn$lga))
lonlatgps$subvillage <- as.numeric(as.factor(datIn$subvillage))

# change 0 for NAs.
lonlatgps$district_code <- ifelse(lonlatgps$latitude==0, NA, lonlatgps$district_code)
lonlatgps$latitude <- ifelse(lonlatgps$latitude==-2e-08, NA, lonlatgps$latitude)
lonlatgps$longitude <- ifelse(lonlatgps$longitude==0, NA, lonlatgps$longitude)
lonlatgps$gps_height <- ifelse(lonlatgps$gps_height==0, NA, lonlatgps$gps_height)
lonlatgps$amount_tsh <- ifelse(lonlatgps$amount_tsh==0, NA, lonlatgps$amount_tsh)
lonlatgps$population <- ifelse(lonlatgps$population==0, NA, lonlatgps$population)

library(missForest)
lonlatgps$festatus <- datIncl$festatus
lolagpnona <- missForest(lonlatgps, verbose=TRUE, parallelize="forest")$ximp
save(lolagpnona, file="lolagpnona_missForest_All.RData")
#load("lolagpnona_missForest_All.RData")

datIncl$district_code <- lolagpnona$district_code
datIncl$latitude <- lolagpnona$latitude
datIncl$longitude <- lolagpnona$longitude
datIncl$gps_height <- lolagpnona$gps_height
datIncl$amount_tsh <- lolagpnona$amount_tsh
datIncl$population <- lolagpnona$population


#Checking plot : Imputed longitued in red, the rest in blue.
datplot <- data.frame(lonori=datIn$longitude, latori=datIn$latitude, lonimp=datIncl$longitude, latimp=datIncl$latitude)
datplot$col <- ifelse(datplot$lonori==0, "red", "blue")
datplot$exp <- ifelse(datplot$lonori==0, 0.8, 0.5)
datplot$tip <- ifelse(datplot$lonori==0, "+", ".")
plot(datplot$lonimp, datplot$latimp, col=datplot$col, cex=datplot$exp,
     pch=datplot$tip, type="p", xlab="Longitude", ylab="Latitude"
     ,main="Longitudes original (blue) and imputed (red)")
# plot(datIncl$longitude, datIncl$latitude, cex=0.5, pch=".", type="p")
# plot(datIn$longitude, datIn$latitude, cex=0.5, pch=".", type="p", xlim=c(30,41))


