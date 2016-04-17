#To sort "ids" as datTesting initially...
library(data.table)
datTmp <- fread("myTest.csv"); datTmp <- as.data.frame(datTmp)
datId <- data.frame(id=datTmp$id)

datId$status_group <- rep("functional", times= nrow(datId))
write.table(datId, file="fileToUpload_all_functional.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

datId$status_group <- rep("non functional", times= nrow(datId))
write.table(datId, file="fileToUpload_all_non_functional.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)

datId$status_group <- rep("functional needs repair", times= nrow(datId))
write.table(datId, file="fileToUpload_all_functional_need_repair.csv",sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)