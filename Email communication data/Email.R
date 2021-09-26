library(Rcpp)
library(RcppArmadillo)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp(file.choose())

data <- read.csv(file.choose(), sep=";")
data[,3] <- as.Date(data[,3])

removed <- c(4,10,21,23,24,26,46,51,75,87,93,111,139)
interval <- c("2010-01-01","2010-02-01","2010-03-01","2010-04-01","2010-05-01",
              "2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01")
y_list <- list()

for(index in 2:length(interval)){
  sub <- data[which(data$EventDate >= interval[index-1] & data$EventDate < interval[index]),]
  
  temp <- matrix(0,nrow=167,ncol=167)
  for(iter in 1:dim(sub)[1]){
    sender <- sub[iter,1]
    receiver <- sub[iter,2]
    if(sender != receiver){ 
      temp[sender,receiver] <- temp[sender,receiver] + 1
    }
  }
  temp <- temp[-removed,]
  temp <- temp[,-removed]
  y_list[[index-1]] <- temp
}

rm(index,iter,receiver,sender,sub,temp,interval,removed,data)

# parameter learning
eta_atleast <- c(0, 0, 0, 0, 0, 0)
eta_atleast <- partial_stepping_atleast_temporal(20, 1000, 1, y_list, eta_atleast, node_attr)
eta_atleast <- newton_raphson_atleast_temporal(10, 10000, 1, y_list, eta_atleast, node_attr)

eta_atmost <- c(0, 0, 0, 0, 0, 0) 
eta_atmost <- partial_stepping_atmost_temporal(20, 1000, 1, y_list, eta_atmost, node_attr)
eta_atmost <- newton_raphson_atmost_temporal(10, 10000, 1, y_list, eta_atmost, node_attr) 
