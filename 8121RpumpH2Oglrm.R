
library(h2o)
#localH2O = h2o.init()
h2o.init(nthreads = -1, max_mem_size = "2G")
#pump.hex <- as.h2o(datIncl, destination_frame="datIncl.hex")

#Transform factors in numeric 
for( i in 1:(ncol(datIncl)-1)) {
  cltmp <- class(datIncl[, i])
  if(cltmp == "factor") {
    datIncl[,i] <- as.numeric( datIncl[,i] )
  } else next
}

datNa <- datIncl
datNa[datNa$latitude==-2e-8,]$longitude <- NA
datNa[datNa$latitude==-2e-8,]$population <- NA
datNa[datNa$latitude==-2e-8,]$amount_tsh <- NA
datNa[datNa$latitude==-2e-8,]$gps_height <- NA
datNa[datNa$latitude==-2e-8,]$latitude <- NA


conna.hex <- as.h2o(datNa, destination_frame="datNa.hex")
sinna.hex <- as.h2o(datIncl, destination_frame="datIncl.hex")

pump.glrm <- h2o.glrm(
                        training_frame = conna.hex, validation_frame = sinna.hex
                       ,cols = 1:(ncol(conna.hex)-1)
                       ,k = 10, init = "SVD", svd_method = "GramSVD"
                       ,loss = "Quadratic", regularization_x = "None"
                       ,regularization_y = "None"
                       ,max_iterations = 2000, min_step_size = 1e-6
                       )

pump.pred <- predict(pump.glrm, conna.hex)
head(pump.pred)

datIncl_recon <- as.data.frame(pump.pred)
nam_val <- names(datIncl_recon)
library(stringr)
new_nam <- str_replace_all(nam_val, "reconstr", "rec")
names(datIncl_recon) <- new_nam

#just coluns with problems.
badpid_row <- rownames(datIncl[datIncl$latitude==-2e-8,])
badpid_row <- as.numeric(badpid_row)
datTemp <- datIncl
datTemp[badpid_row, 'amount_tsh'] <- datIncl_recon[badpid_row, 'rec_amount_tsh']
datTemp[badpid_row, 'gps_height'] <- datIncl_recon[badpid_row, 'rec_gps_height']
datTemp[badpid_row, 'longitude'] <- datIncl_recon[badpid_row, 'rec_longitude']
datTemp[badpid_row, 'population'] <- datIncl_recon[badpid_row, 'rec_population']
datTemp[badpid_row, 'latitude'] <- datIncl_recon[badpid_row, 'rec_latitude']

head(datTemp[badpid_row,])
datIncl <- datTemp

#Use the whole data.frame reconstruided
datIncl_recon$festatus <- datIncl$festatus
nam_rec <- names(datIncl_recon)
nam_rec_new <- str_replace_all(nam_rec,"rec_", "")
names(datIncl_recon) <- nam_rec_new

datIncl <- datIncl_recon



#h2o.shutdown()
