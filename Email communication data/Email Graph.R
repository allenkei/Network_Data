library(igraph)
library(network)
library(sna)
library(ndtv)

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



par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

index <- 9
name <- paste("Month",index)
net2 <- graph_from_adjacency_matrix(y_list[[index]], mode="undirected", weighted = T)
E(net2)$width <- E(net2)$weight/50
plot(net2, vertex.label=NA, vertex.size=7, vertex.frame.color=NA,
     layout = layout.mds, main = name);box(lwd=1)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',
       legend = c("Patient", "Medical Doctor", "Paramedical Staff", "Administrative staff"), 
       col = c("brown1","deepskyblue", "chartreuse3","darkorange"), border = "black",
       pch = c(15,16,16,16),
       xpd = TRUE, horiz = TRUE, cex = 1, bty = 'n')



