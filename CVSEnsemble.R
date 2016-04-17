
setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2015-08 - PumpItUp/documents-export-2015-10-09")

#---------------------------
#Read all .csv files
#---------------------------
library(stringr)
filesTmp <- list.files(path=".", pattern=".csv")
filesTmp <- filesTmp[!str_detect(filesTmp, "xxxx")]


dfcsv <- data.frame()
scordf <- data.frame(score=0)

for(i in 1:length(filesTmp)) {
 scortmp <- word(filesTmp[i], 2, sep=fixed("_")) 
 scordf[i,1] <- scortmp
 
 dftmp <- read.csv(file=filesTmp[i], header=T) 
 if(i==1) {
  dfcsv <- data.frame(rep(0,nrow(dftmp))) 
  dfcsv[,i] <- dftmp[,2]
  names(dfcsv)[1] <- c('V1')
 } else { dfcsv[,i] <- dftmp[,2] }
 if(i==length(filesTmp)) { ids <- dftmp[,1]}
}
#Status
dfcsv <- cbind.data.frame(ids, dfcsv)

scordf <- as.vector(scordf[,1])
scorgd <- as.numeric(scordf)
#Scores
scorgd <- ifelse(scorgd > 1, scorgd/10000, scorgd)


#-----------------------------------------------------------------------
#------------------- CSV ENSEMBLE VOTING  ------------------------------
head(dfcsv)
scorgd
ord_df <- data.frame(nom=names(dfcsv)[2:ncol(dfcsv)], scor=scorgd)
ord_df_end <- ord_df[ order(ord_df$scor, ord_df$nom), ]
filt_value <- 0.8185
ord_df81 <- ord_df_end[ord_df_end$scor >= filt_value , ]



# Columns 1....: Each column has a weight equal to its number.
# Columns should be sorted from low scoring to maximum scoring.
dfcsv_tmp <- dfcsv[, c(2:ncol(dfcsv))]
#dfcsv_redu <- dfcsv_tmp[, as.vector(ord_df_end$nom)]
dfcsv_redu <- dfcsv_tmp[, as.vector(ord_df81$nom)]

# There are some rows with the same values for functional / non functional.
# If I use "apply" I cannot detect that and I cannot get the max value.I should use a "for" loop.
cat_voted_df <- data.frame( id=rep(0, nrow(dfcsv_redu)), status_group=rep(0, nrow(dfcsv_redu)) )
for(i in 1:nrow(dfcsv_redu)) {
  print(i)
valtmp <- dfcsv_redu[i,]

#The sum of indices where they are present is equal to the total weight.
hm_functional <- sum(which(valtmp =="functional"))
hm_nonfunctional <- sum(which(valtmp =="non functional"))
hm_needs <- sum(which(valtmp =="functional needs repair"))

#In some cases sum(functional) is equal a sum(non_functional)
#and there is no way to get the winner category.
if(hm_functional == hm_nonfunctional) {
  hm_nonfunctional <- hm_nonfunctional+1
}
if(hm_functional == hm_needs) {
  hm_needs <- hm_needs+1
}

sum_tmp <- data.frame(
                      functional = hm_functional
                     ,non_functional = hm_nonfunctional
                     ,functional_needs_repair = hm_needs
                 )
cat_voted <- names(sum_tmp)[which(sum_tmp == max(sum_tmp))]
cat_voted_df[i,1] <- dfcsv$ids[i] 
cat_voted_df[i,2] <- cat_voted 

}
head(cat_voted_df)
dim(cat_voted_df)
library(stringr)
cat_voted_df$status_group <- str_replace_all(cat_voted_df$status_group, "_"," ")
head(cat_voted_df)
dim(cat_voted_df)

datSubEnd <- t(prop.table(table(cat_voted_df$status_group))*100)
tabEnd <- rbind(datSubEnd, c(54.61, 7.19, 38.20))
rownames(tabEnd) <- c('Sub','Real')
tabEnd <- as.data.frame(tabEnd)
tabEnd[3,1] <- abs(tabEnd[1,1]-tabEnd[2,1])
tabEnd[3,2] <- abs(tabEnd[1,2]-tabEnd[2,2])
tabEnd[3,3] <- abs(tabEnd[1,3]-tabEnd[2,3])
rownames(tabEnd)[3] <- "diff"
sumEr <- round(sqrt(sum(tabEnd[3,]^2)),3) #sqrt(sum(var1^2 + var2^2...))
tabEnd <- rbind(tabEnd, c(sumEr,sumEr,sumEr))
rownames(tabEnd)[4] <- "sumDiff"
tabEnd

hm_ensem <- ncol(dfcsv_redu)
timval <- str_replace_all(Sys.time(), " |:", "_")
write.table(cat_voted_df, file=paste(
  "Res_xxxx_Ensemble_n",hm_ensem,"_Filt_",filt_value,"_Err_",sumEr,"_",timval,".csv",sep="")
  ,sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE
)

