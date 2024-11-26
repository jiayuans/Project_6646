library(tidyverse)
library(coda)
library(rjags)
library(runjags)
library(mcmcplots)

dirg <- "C:/UCHealth/Course/Fall 2024/BIOS 6646-Survival Analysis/Project/Project_6646/"
setwd(dirg)
##################################################################
##    Functions to Read data
## 
##################################################################

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
X1=c(rep(1,N/2),rep(0,N/2))

###set number of iterations#################################
I=50
set.seed(123)


#########################################################################
# Function that generates observations from a NHPP- returns event times
# Input: parameters for the mean of a poisson process: a(shape parameter),b, T (exposure time)
# Output: REturns the event times and a variable that indicates whether the observation is an event or a 
#		  censoring time (no events observed in the whole interval); status=1 indicates event and 0 censoring

NHPP<-function(a,b,T){
  mu <- b*T^a  # Mean of the Poisson process up to time T
  n <-rpois(1, mu)  #  number of events poisson
  if (n!=0) {
    u <- runif(n,0,1) # n uniforms
    u <- sort(u)
    y <- T*u^(1/a) 
    y[length(y)+1] <- T
    y_0 <- rep(NA,length(y))
    for (i in 2:length(y_0)){
      y_0[i] <- y[i-1]
    }
    y_0[which(is.na(y_0)==TRUE)] <- 0
    return(cbind(y_0,y,c(rep(1,length(y)-1),0),n))    #returns n event times
  } else 
    return(cbind(0,T,0,n)) 
}

#########################################################################
# Function that creates an event times dataset for a poisson process (continuous data )
# Input: parameters for the intensity function alpha; beta; beta0; x; ga (association parameter); Tei 
# Output: A dataset with variables
# 		 id, xi (treatment),Tei, time, status
# -------------- Building the simulated poisson data -----
poisson.d <- function(alpha,beta,beta0,x,ph,TTei){
  le <- length(x)
  vi <- ifelse(rep(ph,le)==rep(0,le),rep(1,le),rgamma(le,shape=1/ph, scale=ph))
  
  times <- NHPP(b=vi[1]*exp(beta*x[1])*exp(beta0),a=alpha,T=TTei[1])
  start <-  times[,1]
  stop <- times[,2]
  status <- times[,3]
  n.rec <- times[,4]
  id <- rep(1,length(stop))
  xi <- rep(x[1],length(stop))
  Tei <- rep(TTei[1],length(stop))
  for (i in 2:length(x)){
    times2 <- NHPP(b=vi[i]*exp(beta0+beta*x[i]),a=alpha,T=TTei[i]) 
    start2 <-  times2[,1]
    stop2 <- times2[,2]
    status2 <- times2[,3]
    n.rec2 <- times2[,4]
    id <- c(id,rep(i,length(stop2)))
    xi <- c(xi,rep(x[i],length(stop2)))
    Tei <- c(Tei,rep(TTei[i],length(stop2)))
    
    start <- c(start,start2)
    stop <- c(stop,stop2)
    status <- c(status,status2)
    n.rec <- c(n.rec,n.rec2)
  }
  return(data.frame(id,xi,Tei,n.rec,start,stop,status))
}

#######################################################
for (r in 1:I){
  simdat.pe00 <- poisson.d(alpha=1.7,beta=0.25,beta0=-1.35,x=X1,ph=.5,TTei=tt)
  
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
        L.e[i] <- ifelse(Ti[i,1]!=0, prod(lambda[i,1:k.pe[i]]) * exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)), exp(v[i]*exp(b0+b*X1[i])*(time.t0[i]^a-time.tau[i]^a)))
        ll.e[i] <- log(L.e[i])
        phi[i] <- -log(L.e[i]) + 1000
        zeros[i] ~ dpois(phi[i])
  }
  log_lik0.e <- sum(ll.e[]) 
  dev.e <- -2*log_lik0.e
  ## prior distributions
  a ~ dgamma(0.01,0.01)
  b0 ~ dnorm(0,0.0001)	
  b ~ dnorm(0,0.0001)		
	ph ~ dgamma(0.001,0.001)
}"


####Observed DATA
data <- dump.format(list(N=N, X1=X1,k.pe=k.pe, time.t0=time.t0, time.tau=time.tau, Ti=Ti)) 
###initial Values
inits1 <- dump.format(list(b0=-1.35, b=0.25, a=1.7, ph=.5,
                           .RNG.name="base::Super-Duper", .RNG.seed=1))
inits2 <- dump.format(list(b0=-1.36,b=0.26, a=1.71, ph=.5,
                           .RNG.name="base::Super-Duper", .RNG.seed=2))
#### Run the model and produce plots
res <- run.jags(model=modelrancp, burnin=5000, sample=5000, 
                monitor=c("b0","b","a","ph","v","ll.e","dev.e","dic"), 
                data=data, n.chains=2, inits=c(inits1,inits2), thin=2, module='dic')

summary <- summary(res)
result_df <- as.data.frame(summary)
write.csv(result_df, paste0("result.",r,".csv"))

res_jm <- res$mcmc
vars<-mcmc.list(res_jm[[1]][,c(1:16)],res_jm[[2]][,c(1:16)])
pdf(file = paste0("traceplot.",r,".pdf"),   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches
traplot(vars)
dev.off()
} 


###########################################################################
# Read the output csv files
text <- list.files(pattern="result.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("result.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ph.mean<-rep(NA,I)
v.mean<-rep(NA,I)

for(i in 1:I){ 
  Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.2,1,0)
  b0.mean[i] <-data_frames[[i]][1,5] 
  b1.mean[i] <-data_frames[[i]][2,5] 
  a.mean[i] <-data_frames[[i]][3,5] 
  ph.mean[i] <-data_frames[[i]][4,5] 
  v.mean[i] <-mean(data_frames[[i]][5:404,5])
}

Sim.results=cbind(Flag,b0.mean,b1.mean,a.mean,ph.mean,v.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)
