library(Rcpp)
library(RcppArmadillo)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp(file.choose())

node <- read.csv(file.choose())
edge <- read.csv(file.choose())
colnames(edge) <- c("sender","receiver","time")
node_attr <- ifelse(node$status=='PAT',1,ifelse(node$status=='MED',2,ifelse(node$status=='NUR',3,4)))

for(iter in 1:32424){
  if(edge[iter,1] > edge[iter,2]){
    replacement <- edge[iter,1]
    edge[iter,1] <- edge[iter,2]
    edge[iter,2] <- replacement
  }
}
nrow(edge[which(edge$sender > edge$receiver),])

edge <- edge[order(edge$sender,edge$receiver,edge$time),]

y_list <- list()
start_time <- 1
for(iter in 1:4){
  track_i <- track_j <- 100 # initialize tracker
  track_time <- 0
  end_time <- start_time + 24*60*60 # 24 hours later
  
  edge_temp <- edge[which(edge$time >= start_time & edge$time < end_time),]
  
  y <- matrix(0,nrow=75,ncol=75)
  for(row_idx in 1:dim(edge_temp)[1]){
    i <- edge_temp[row_idx,1]+1
    j <- edge_temp[row_idx,2]+1
    cur_time <- edge_temp[row_idx,3]
    
    if(i != track_i | j != track_j){
      track_i <- i
      track_j <- j
      y[i,j] <- y[i,j] + 1
      y[j,i] <- y[j,i] + 1
      track_time <- cur_time
    }else if(i == track_i & j == track_j){
      if(cur_time - track_time > 20){
        y[i,j] <- y[i,j] + 1
        y[j,i] <- y[j,i] + 1
      }
      track_time <- cur_time
    }
    
  }
  y_list[[iter]] <- y
  
  start_time <- end_time
}

rm(edge_temp, y, end_time, i,j, iter, row_idx, start_time, track_i, track_j, cur_time, track_time, replacement)

# parameter learning
eta_atleast <- rep(0,9)
eta_atleast <- partial_stepping_atleast_temporal(20, 5000, 1, y_list, eta_atleast, node_attr) 
eta_atleast <- newton_raphson_atleast_temporal(10, 20000, 1, y_list, eta_atleast, node_attr) 

eta_atmost <- rep(0,9)
eta_atmost <- partial_stepping_atmost_temporal(20, 5000, 1, y_list, eta_atmost, node_attr)
eta_atmost <- newton_raphson_atmost_temporal(10, 20000, 1, y_list, eta_atmost, node_attr) 
