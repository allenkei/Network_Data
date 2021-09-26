library(igraph)
library(network)
library(sna)
library(ndtv)

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


rm(edge_temp, y, end_time, i,j, iter, row_idx, start_time, track_i, track_j, 
   cur_time, track_time, replacement)

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
net2 <- graph_from_adjacency_matrix(y_list[[4]], mode="undirected", weighted = T)
V(net2)$color <- ifelse(node$status=='PAT','brown1',
                        ifelse(node$status=='MED','deepskyblue',
                               ifelse(node$status=='NUR','chartreuse3','darkorange')))
V(net2)$shape <- ifelse(node$status=='PAT','square',
                        ifelse(node$status=='MED','circle',
                               ifelse(node$status=='NUR','circle','circle')))
E(net2)$width <- E(net2)$weight/10
plot(net2, vertex.label=NA, vertex.size=7, vertex.frame.color=NA,
     layout = layout.mds, main = 'Day 4');box(lwd=1)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',
       legend = c("Patient", "Medical Doctor", "Paramedical Staff", "Administrative Staff"), 
       col = c("brown1","deepskyblue", "chartreuse3","darkorange"), border = "black",
       pch = c(15,16,16,16),
       xpd = TRUE, horiz = TRUE, cex = 1, bty = 'n')
