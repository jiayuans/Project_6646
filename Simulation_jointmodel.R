#!/usr/bin/env Rscript
#setwd("C:/UCHealth/Course/Fall 2024/BIOS 6646-Survival Analysis/Project/Project_6646")
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

t<-round(first.tt)
tt<-round(last.tt)
k.pa<-(tt-t)*4

X1=c(rep(1,N/2),rep(0,N/2))
##X1=sample(c(1,0),N, replace = TRUE)

#############################################################
X <- as.matrix(read.csv(list.files(pattern="X_data.")))
Y <- as.matrix(read.csv(list.files(pattern="Y_data.")))
simdat.pe00 <- as.data.frame(read.csv(list.files(pattern="sim.pe_data.")))
#############################################################

tt<-tt-0.25
  timeS <- as.data.frame(cbind(id,t)) ## non-informative left truncation times
  timeE <- as.data.frame(cbind(id,tt))
  
  simdat.pe0 <- merge(simdat.pe00, timeS,all=TRUE)
  
  ## Simulate informative left truncation times
  simdat.pe01 <- simdat.pe0 %>%
    group_by(id) %>%
    mutate(min.stop = min(stop)) %>% 
    ungroup()  # Ungroup to remove the grouping structure
  
  ## t_i1
  min.stop_vector <- simdat.pe01 %>%
    select(id, min.stop) %>%
    distinct() %>%
    pull(min.stop)
  
  eta <- -0.3
  sigma <- 0.1  
  
  set.seed(123)
  lt <- exp(log(min.stop_vector) + eta + rnorm(N, mean = 0, sd = sigma))
  sum(lt<tt) #400
  
  simdat.pe02 <- merge(data.frame(id=1:400,lt),simdat.pe01,by="id")
  
  simdat.pe <- subset(simdat.pe02, stop >= lt)
  timeS_ <- as.data.frame(cbind(id,lt))
  
  time <- subset(simdat.pe,status==1)
  time1 <- time[,c("id","stop")]  
  simdat.pe1 <- merge(timeS_,timeE,all=TRUE)
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
  time.t0 <- lt
  time.tau <- tt
  
  #################input variables for simulation#####################
  #### checking for how many individuals we have NAs in the middle of followup
  sum.na <- rep(NA,N)
  k.pe=rep(NA,N)
  
  ids <- unique(Y.epic$id) ## 103104 103125 103129 103145 103147
  for (i in 1:N){
    na.indices <- which(Y.epic$lt[Y.epic$id==ids[i]] %in% NA)
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
        for(j in 1:k.pa[i]){
  ### PA model
        Y[i,j] ~ dbin(p2[i,j],1)
        logit(p2[i,j]) <- c0 + c[1] * (X[i,j]-cp1) + c[2] * (X[i,j]-cp1) * (2*step(X[i,j]-cp1)-1) + c[3] * (X[i,j]-cp2) * (2*step(X[i,j]-cp2)-1) + c[4] * X1[i] + u[i]
        }
        for(j in 1:k.pe[i]){
  ### PE model
       ## Weibull baseline
        lambda0[i,j] <- a*(Ti[i,j])^(a-1)
        lambda[i,j] <- lambda0[i,j]*v[i]*exp(b0+b*X1[i])
       }
        u[i] ~ dnorm(0,u.tau)
        L.a[i] <- prod(((p2[i,1:k.pa[i]])^(Y[i,1:k.pa[i]]))*((1-p2[i,1:k.pa[i]])^(1-Y[i,1:k.pa[i]])))
        ll.a[i] <- log(L.a[i])
        w[i] ~ dnorm(0,w.tau)
        v[i] <- exp(ga*u[i]+w[i])
        time.t0[i] <-  exp(log(time.t1[i]) + eta + epsilon)
        L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        ll.e[i] <- log(L.e[i])
        phi[i] <- -log(L.e[i]) + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.a <- sum(ll.a[])
  log_lik0.e <- sum(ll.e[]) 
  dev.a <- -2*log_lik0.a
  dev.e <- -2*log_lik0.e
  c0 ~ dnorm(0,0.0001)
	for (k in 1:4){
	      c[k] ~ dnorm(0,0.0001)	
	}
  ## prior distributions
	u.tau ~ dgamma(0.001,0.001)
	cp1 ~ dnorm(cp1.mu,cp1.tau)	
	cp2.temp ~ dunif(0,max)
	cp2 <- cp1 + cp2.temp
	cp1.mu ~ dnorm(0,0.001)
	cp1.tau ~ dgamma(0.001,0.001)
	B1 <-c[1]-c[2]-c[3]
  B2 <-c[1]+c[2]-c[3]
  B3 <-c[1]+c[2]+c[3]
  u.tau.inv <- 1/u.tau  ## variance 
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.0001)	
  b ~ dnorm(0,0.0001)		
	ga ~ dnorm(0,0.0001)
	w.tau ~ dgamma(0.001,0.001)
	w.tau.inv <- 1/w.tau  ## variance 
  epsilon ~ dnorm(0,eps.tau)	
  eps.tau ~ dgamma(0.001,0.001)
  eps.tau.inv <- 1/eps.tau  ## variance 
  eta ~ dnorm(0,0.0001)	
}"

  
  ####Observed DATA
  data <- dump.format(list(X=X, Y=Y, N=N, k.pa=k.pa, max=max(tt),
                           X1=X1, k.pe=k.pe, time.t1=min.stop_vector, time.tau=time.tau, Ti=Ti)) 
  ###initial Values
  inits1 <- dump.format(list(c0=-4.6, c=c(0.1,0.17,0.1,0.1), u.tau=1, cp1=4.5, cp2.temp=10,
                             b0=-4.5, b=0.25, a=1.8, w.tau=1, ga=0.25, eta=-0.3, eps.tau=100,  
                             .RNG.name="base::Super-Duper", .RNG.seed=1))
  inits2 <- dump.format(list(c0=-4.61, c=c(0.1,0.17,0.1,0.1)+0.01, u.tau=1, cp1=4.6, cp2.temp=10,
                             b0=-4.51, b=0.26, a=1.81, w.tau=1, ga=0.26, eta=-0.3, eps.tau=100, 
                             .RNG.name="base::Super-Duper", .RNG.seed=2))

  #### Run the model and produce plots
  res <- run.jags(model=modelrancp, burnin=20000, sample=1000, 
                  monitor=c("B1","B2","B3","cp1","cp2","c0","c","u.tau.inv",
                            "b0","b","a","ga","w.tau.inv","eta","sigma",
                            "u","v","w",
                            "u.tau","w.tau","cp1.mu","cp1.tau","cp2.temp","ll.a","ll.e","dev.a","dev.e","dic"), 
                  data=data, n.chains=2, inits=c(inits1,inits2), thin=10, module='dic')
  
  summary <- summary(res)
  result_df <- as.data.frame(summary)
  text <- list.files(pattern="X_data.")
  num <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[2]]))
  write.csv(result_df, paste0("result.",num,".csv"))
  
  res_jm <- res$mcmc
  #dimnames(res_jm[[1]])
  vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
  #str(vars)
  #plot(vars[,1])
  #summary(vars)
  pdf(file = paste0("traceplot.",num,".pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  traplot(vars)
  dev.off()
   