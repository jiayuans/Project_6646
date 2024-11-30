#!/usr/bin/env Rscript
library(coda)
library(rjags)
library(runjags)
library(tidyverse)
library(mcmcplots)

long.time <- read.csv("long.data.csv")
first.tt <- long.time[,2]
last.tt <- long.time[,3]

####time of first visit and last visit#######
N<-length(last.tt)
#participant ID
id<-rep(1:N)
length(id)

#t<-round(first.tt)
tt<-round(last.tt)
X1=c(rep(1,N/2),rep(0,N/2))

set.seed(123)

#############################################################
t <- as.data.frame(read.csv(list.files(pattern="t_data.")))[,1]
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="rec.sim.pe_data.")))
#############################################################

timeS <- as.data.frame(cbind(id,t)) ## left truncation time
timeE <- as.data.frame(cbind(id,tt))

simdat.pe0 <- merge(simdat.pe00, timeS,all=TRUE)
simdat.pe <- subset(simdat.pe0, stop >= t)

time <- subset(simdat.pe,status==1)
time1 <- time[,c("id","stop")]  
simdat.pe1 <- merge(timeS,timeE,all=TRUE)
simdat.pe2 <- merge(simdat.pe1,time1,all=TRUE)
simdat.pe2$stop[which(is.na(simdat.pe2$stop))] <- 0

#length(which(simdat.pe2$stop== 0))

count <- simdat.pe2 %>% count(id)
max.count <- max(count$n) 

##########################Assigning unique number to each subject##########################
simdat.pe3 <- simdat.pe2 %>% group_by(id) %>% mutate(time = c(1:length(id)))
Yd.temp <- data.frame(id = rep(unique(simdat.pe00$id),each=max.count), time = 1:max.count) 
Y.epic <- merge(simdat.pe3,Yd.temp,by=c('id','time'),all.y=TRUE)

#################Readingin data for time matrix#############################
Ti <- matrix(Y.epic$stop, N, max.count, byrow=TRUE)

#################Readingin data for X, t0, tau vectors#############################
##X1 <- as.numeric(X.dat.pe[,2]) ## sexf: female
time.t0 <- t
time.tau <- tt

#################input variables for simulation#####################
#### checking for how many individuals we have NAs in the middle of followup
sum.na <- rep(NA,N)
k.pe=rep(NA,N)

ids <- unique(Y.epic$id) ## 103104 103125 103129 103145 103147
for (i in 1:N){
  na.indices <- which(Y.epic$t[Y.epic$id==ids[i]] %in% NA)
  if (length(na.indices)==0){
    k.pe[i] <- max.count} else{
      k.pe[i] <- min(na.indices)-1}
}

  ############Model in the JAGS format#####################
  ############Two fixed CP#####################  
  modelrancp <- "
data { 
  for(i in 1:N){
       zeros[i]<- 0
  }
}
model { 
  for(i in 1:N){ 
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda[i,j] <- lambda0[i,j]*v[i]*exp(b0+b*X1[i])
        }
        v[i] ~ dgamma(1/ph,1/ph) ## include ph with priors ~ gamma 
        time.t0[i] ~ dexp(1/(ga1*v[i]+ga0))
        L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        ll.e[i] <- log(L.e[i])
        phi[i] <- -log(L.e[i]) + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.e <- sum(ll.e[]) 
  dev.e <- -2*log_lik0.e
  ## prior distributions
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.0001)	#b0 ~ dnorm(0,0.1)	
  b ~ dnorm(0,0.0001)		#b ~ dnorm(0,0.1)
	ph ~ dgamma(0.001,0.001) #ph ~ dgamma(0.01,0.01)
	ga1 ~ dgamma(0.01, 0.01)	 #ga1 ~ dnorm(0,0.0001)
	ga0 ~ dgamma(0.01, 0.01)	 #ga0 ~ dnorm(0,0.0001)
}"

  
  ####Observed DATA
  data <- dump.format(list(N=N, X1=X1,k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti)) 
  ###initial Values
  inits1 <- dump.format(list(b0=-1.35, b=0.25, a=1.7, ph=.5, ga0=0.8, ga1=1,
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(b0=-1.36,b=0.26, a=1.71, ph=.5, ga0=0.8, ga1=1,
                             .RNG.name="base::Super-Duper", .RNG.seed=2))
  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=5000, sample=5000, 
                  monitor=c("b0","b","a","ph","ga0","ga1", "v","ll.e","dev.e","dic"), 
                  data=data, n.chains=2, inits=c(inits1,inits2), thin=10, module='dic')
    
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="rec.sim.pe_data.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[4]]))
  write.csv(result_df, paste0("rec.result_it.",num,".csv"))
  
  res_jm <- res$mcmc
  vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  pdf(file = paste0("rec.traceplot_it.",num,".pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  traplot(vars)
  dev.off()