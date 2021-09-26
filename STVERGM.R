library(Rcpp)
library(RcppArmadillo)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

####################################
# Experiment 1: Parameter Matching #
####################################

sourceCpp(file.choose())
node_attr <- rep(1,50)
network_before <- as.matrix(read.csv(file.choose()))
y_init <- matrix(1,nrow=50,ncol=50); diag(y_init) <- 0
eta_atleast <- c(-1,2,1)
eta_atmost <- c(2,-1,1)

MCMC_list_atleast <- gen_MCMC_valued_atleast(10000, eta_atleast, network_before, node_attr, y_init)
MCMC_list_atmost <- gen_MCMC_valued_atmost(10000, eta_atmost, network_before, m=5, node_attr, y_init)

# check convergence
test_atleast <- gen_features_MCMC_valued_atleast(MCMC_list_atleast, network_before, node_attr, length(eta_atleast))
avg_network_feature_atleast <- colMeans(test_atleast[9991:10000,])

test_atmost <- gen_features_MCMC_valued_atmost(MCMC_list_atmost, network_before, node_attr, length(eta_atmost))
avg_network_feature_atmost <- colMeans(test_atmost[9991:10000,])
  
par_iter <- 1000
atleast_holder <- matrix(0,nrow=par_iter,ncol=length(eta_atleast))

for(iter in 1:par_iter){
  
  network_augmentation <- MCMC_list_atleast[[10001-iter]]
  g_obs_atleast <- gen_feature_valued_atleast(network_augmentation, network_before, node_attr)
  eta_atleast <- c(0, 0, 0)
  eta_atleast <- partial_stepping_atleast_short(20, 1000, 1, network_augmentation, g_obs_atleast, network_before, eta_atleast, node_attr)
  eta_atleast <- newton_raphson_atleast_short(10, 5000, 1, network_augmentation, g_obs_atleast, network_before, eta_atleast, node_attr)
  atleast_holder[iter,] <- c(eta_atleast)
  
}

atmost_holder <- matrix(0,nrow=par_iter,ncol=length(eta_atmost))

for(iter in 1:par_iter){
  
  network_diminution <- MCMC_list_atmost[[10001-iter]]
  g_obs_atmost <- gen_feature_valued_atmost(network_diminution, network_before, node_attr)
  eta_atmost <- c(0, 0, 0)
  eta_atmost <- partial_stepping_atmost_short(20, 1000, 1, network_diminution, g_obs_atmost, network_before, eta_atmost, 5, node_attr)
  eta_atmost <- newton_raphson_atmost_short(10, 5000, 1, network_diminution, g_obs_atmost, network_before, eta_atmost, 5, node_attr)
  atmost_holder[iter,] <- c(eta_atmost)
  
}


colMeans(atleast_holder) - c(-1,2,1)
apply(atleast_holder,2,sd)

colMeans(atmost_holder) - c(2,-1,1)
apply(atmost_holder,2,sd)




###############################################
# Experiment 2: Parameter Matching (temporal) #
###############################################

sourceCpp(file.choose())
node_attr <- read.csv(file.choose())[,1]
network1 <- as.matrix(file.choose())

y_list <- list(network1)
eta_atleast <- c(-2, 1.4, 0.4, 1)
eta_atmost <- c(-1, 0.8, 0.4, 1)
MCMC_length <- 200

for(t in 2:20){
  y_init <- matrix(1,nrow=dim(network1)[1],ncol=dim(network1)[2]);diag(y_init) <- 0;
  MCMC_list_atleast <- gen_MCMC_valued_atleast(MCMC_length, eta_atleast, y_list[[t-1]], node_attr, y_init)
  MCMC_list_atmost <- gen_MCMC_valued_atmost(MCMC_length, eta_atmost, y_list[[t-1]], m=3, node_attr, y_init)
  y_list[[t]] <- construct_network_after(y_list[[t-1]],MCMC_list_atleast[[MCMC_length]],MCMC_list_atmost[[MCMC_length]])
}

par_iter <- 1000
atleast_holder <- atmost_holder <- matrix(0,nrow=par_iter,ncol=4)
for(i in 1:par_iter){
  
  eta_atleast <- c(0, 0, 0, 0)
  eta_atleast <- partial_stepping_atleast_temporal(20, 200, 1, y_list, eta_atleast, node_attr)
  atleast_holder[i,] <- newton_raphson_atleast_temporal(5, 2000, 1, y_list, eta_atleast, node_attr)
  
  eta_atmost <- c(0, 0, 0, 0)
  eta_atmost <- partial_stepping_atmost_temporal(20, 200, 1, y_list, eta_atmost, node_attr)
  atmost_holder[i,] <- newton_raphson_atmost_temporal(5, 2000, 1, y_list, eta_atmost, node_attr)
}

colMeans(atleast_holder) - c(-2, 1.4, 0.4, 1)
apply(atleast_holder,2,sd)

colMeans(atmost_holder) - c(-1, 0.8, 0.4, 1)
apply(atmost_holder,2,sd)


