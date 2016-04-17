

#----------------------------------
#------------------------- TRAIN
changeTrain <- function() {
vfreq <- 5
# Train - funder
a <- as.data.frame(sort(table(datIn$funder),decreasing=TRUE))
names(a) <- c("Freq")
a$who <- rownames(a)
rownames(a) <- NULL
agd <- a[a$Freq>=vfreq,]
fefunder <- ifelse(datIn$funder %in% agd$who, datIn$funder, "other")

# Train - installer
a <- as.data.frame(sort(table(datIn$installer),decreasing=TRUE))
names(a) <- c("Freq")
a$who <- rownames(a)
rownames(a) <- NULL
agd <- a[a$Freq>=vfreq,]
feinstaller <- ifelse(datIn$installer %in% agd$who, datIn$installer, "other")

# Train - wpt_name
a <- as.data.frame(sort(table(datIn$wpt_name),decreasing=TRUE))
names(a) <- c("Freq")
a$who <- rownames(a)
rownames(a) <- NULL
agd <- a[a$Freq>=vfreq,]
fewpt_name <- ifelse(datIn$wpt_name %in% agd$who, datIn$wpt_name, "other")

# Train - subvillage
a <- as.data.frame(sort(table(datIn$subvillage),decreasing=TRUE))
names(a) <- c("Freq")
a$who <- rownames(a)
rownames(a) <- NULL
agd <- a[a$Freq>=vfreq,]
fesubvillage <- ifelse(datIn$subvillage %in% agd$who, datIn$subvillage, "other")

# Train - ward
a <- as.data.frame(sort(table(datIn$ward),decreasing=TRUE))
names(a) <- c("Freq")
a$who <- rownames(a)
rownames(a) <- NULL
agd <- a[a$Freq>=vfreq,]
feward <- ifelse(datIn$ward %in% agd$who, datIn$ward, "other")


# Train - scheme_name
a <- as.data.frame(sort(table(datIn$scheme_name),decreasing=TRUE))
names(a) <- c("Freq")
a$who <- rownames(a)
rownames(a) <- NULL
agd <- a[a$Freq>=vfreq,]
fescheme_name <- ifelse(datIn$scheme_name %in% agd$who, datIn$scheme_name, "other")

return(data.frame(fefunder, feinstaller, fewpt_name, fesubvillage, feward, fescheme_name))
}


#----------------------------------
#------------------------- TESTING
changeTest <- function() {
vfreq <- 5
  # Testing - funder
  a <- as.data.frame(sort(table(datIn$funder),decreasing=TRUE))
  names(a) <- c("Freq")
  a$who <- rownames(a)
  rownames(a) <- NULL
  agd <- a[a$Freq>=vfreq,]
  fefunder <- ifelse(datTestingori$funder %in% agd$who, datTestingori$funder, "other")
  
  # Testing - installer
  a <- as.data.frame(sort(table(datIn$installer),decreasing=TRUE))
  names(a) <- c("Freq")
  a$who <- rownames(a)
  rownames(a) <- NULL
  agd <- a[a$Freq>=vfreq,]
  feinstaller <- ifelse(datTestingori$installer %in% agd$who, datTestingori$installer, "other")
  
  # Testing - wpt_name
  a <- as.data.frame(sort(table(datIn$wpt_name),decreasing=TRUE))
  names(a) <- c("Freq")
  a$who <- rownames(a)
  rownames(a) <- NULL
  agd <- a[a$Freq>=vfreq,]
  fewpt_name <- ifelse(datTestingori$wpt_name %in% agd$who, datTestingori$wpt_name, "other")
  
  # Testing - subvillage
  a <- as.data.frame(sort(table(datIn$subvillage),decreasing=TRUE))
  names(a) <- c("Freq")
  a$who <- rownames(a)
  rownames(a) <- NULL
  agd <- a[a$Freq>=vfreq,]
  fesubvillage <- ifelse(datTestingori$subvillage %in% agd$who, datTestingori$subvillage, "other")
  
  # Testing - ward
  a <- as.data.frame(sort(table(datIn$ward),decreasing=TRUE))
  names(a) <- c("Freq")
  a$who <- rownames(a)
  rownames(a) <- NULL
  agd <- a[a$Freq>=vfreq,]
  feward <- ifelse(datTestingori$ward %in% agd$who, datTestingori$ward, "other")
  
  
  # Testing - scheme_name
  a <- as.data.frame(sort(table(datIn$scheme_name),decreasing=TRUE))
  names(a) <- c("Freq")
  a$who <- rownames(a)
  rownames(a) <- NULL
  agd <- a[a$Freq>=vfreq,]
  fescheme_name <- ifelse(datTestingori$scheme_name %in% agd$who, datTestingori$scheme_name, "other")
  
return(data.frame(fefunder, feinstaller, fewpt_name, fesubvillage, feward, fescheme_name))
}



# V1    V2    V3   V4
# # 1       funder  1898   981  243
# # 2    installer  2146  1092  265
# # 3     wpt_name 37400 10840 8284
# # 5   subvillage 19288  8444 2138
# # 8         ward  2092  1959    6
# # 11 scheme_name  2697  1790  172

