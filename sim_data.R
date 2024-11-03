library(tidyverse)

dirg <- "C:/UCHealth/Course/Fall 2024/BIOS 6646-Survival Analysis/Project/Project_6646"
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
k.pa<-(tt-t)*4
kk=max(k.pa)

###set number of iterations#################################
I=201

###############set true values#########################################
c0=-4.5 #-4.6
c1=0.1
c2=0.2 #0.17
c3=0.1
c4=0.1
Verror=1
cp1.true=4.5
cp2.true=14.5

#############################################################
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
poisson.d <- function(alpha,beta,beta0,x,ga,TTei){
  le <- length(x)
  c_0i <- rnorm(le,0,1) #1.3
  vi <- exp(ga*b_0i+c_0i)
  ##vi <- ifelse(rep(ph,le)==rep(0,le),rep(1,le),rgamma(le,shape=1/ph, scale=ph))
  
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
for (r in 2:I){
  b_0i<-rnorm(N,0,1) #1.6
  X1=c(rep(1,N/2),rep(0,N/2))
  ##X1=sample(c(1,0),N, replace = TRUE)
  
  I1<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  I2<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  p2<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  Y<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  X<-matrix(NA, nrow=N, ncol=kk, byrow=TRUE)
  for (i in 1:N){
    X[i,1:k.pa[i]]<-c(seq(t[i],tt[i]-0.25,0.25))
  }
  
  for (i in 1:N){
    for (j in 1:k.pa[i]){
      I1[i,j]<-ifelse(X[i,j]< cp1.true,-1,1)
      I2[i,j]<-ifelse(X[i,j]< cp2.true,-1,1)
      p2[i,j]=exp(c0+c1*(X[i,j]-cp1.true)+c2*(X[i,j]-cp1.true)*I1[i,j]+c3*(X[i,j]-cp2.true)*I2[i,j]+c4*X1[i]+b_0i[i])/(1+exp(c0+c1*(X[i,j]-cp1.true)+c2*(X[i,j]-cp1.true)*I1[i,j]+c3*(X[i,j]-cp2.true)*I2[i,j]+c4*X1[i]+b_0i[i]))
      Y[i,j]=rbinom(Verror, 1, p2[i,j])
    }
  }
  
  simdat.pe00 <- poisson.d(alpha=1.8,beta=0.25,beta0=-4.5,x=X1,ga=0.25,TTei=tt-0.25)
  
  X_df <- as.data.frame(X)
  filename <- paste0("X_data.", r-2, ".csv")
  write.csv(X_df, file = filename, row.names = FALSE)
  
  Y_df <- as.data.frame(Y)
  filename <- paste0("Y_data.", r-2, ".csv")
  write.csv(Y_df, file = filename, row.names = FALSE)
  
  simdat.pe_df <- as.data.frame(simdat.pe00)
  filename <- paste0("sim.pe_data.", r-2, ".csv")
  write.csv(simdat.pe_df, file = filename, row.names = FALSE)
} 
 