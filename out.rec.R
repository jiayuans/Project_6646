setwd("C:/Users/jiayu/OneDrive/Desktop/BIOS6646_non_it")

###########################################################################
# Read csv files
text <- list.files(pattern="rec.result0.")
num <- as.numeric(unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[3]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("rec.result0.", i, ".csv") 
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
Flag[i] <- ifelse(max(data_frames[[i]][,12])<1.1,1,0)
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

#> round(colMeans(Sim.results.1),2)
#Flag b0.mean b1.mean  a.mean ph.mean  v.mean 
#1.00   -1.34    0.25    1.70    0.50    1.00 
#> round(colMeans(Sim.results),2)
#Flag b0.mean b1.mean  a.mean ph.mean  v.mean 
#0.95   -1.35    0.25    1.70    0.50    1.00 


## Metrics:
b0=rep(-1.35,I)
b1=rep(0.25,I)
a=rep(1.7,I) 
ph=rep(0.5,I)

dat <- as.data.frame(cbind(Sim.results,b0,b1,a,ph))
bias <- c(sum(dat$b0.mean-dat$b0)/I, sum(dat$b1.mean-dat$b1)/I, sum(dat$a.mean-dat$a)/I, sum(dat$ph.mean-dat$ph)/I)
mse <- c(sum((dat$b0.mean-dat$b0)^2)/I, sum((dat$b1.mean-dat$b1)^2)/I, sum((dat$a.mean-dat$a)^2)/I, sum((dat$ph.mean-dat$ph)^2)/I)

b0.low<-rep(NA,I)
b1.low<-rep(NA,I)
a.low<-rep(NA,I)
ph.low<-rep(NA,I)

for(i in 1:I){ 
  b0.low[i] <-data_frames[[i]][1,2] 
  b1.low[i] <-data_frames[[i]][2,2] 
  a.low[i] <-data_frames[[i]][3,2] 
  ph.low[i] <-data_frames[[i]][4,2]
}


b0.high<-rep(NA,I)
b1.high<-rep(NA,I)
a.high<-rep(NA,I)
ph.high<-rep(NA,I)


for(i in 1:I){ 
  b0.high[i] <-data_frames[[i]][1,4] 
  b1.high[i] <-data_frames[[i]][2,4] 
  a.high[i] <-data_frames[[i]][3,4] 
  ph.high[i] <-data_frames[[i]][4,4]
}

dat1 <- as.data.frame(cbind(dat,b0.low,b1.low,a.low,ph.low,b0.high,b1.high,a.high,ph.high))

dat1$b0.cp <- ifelse(dat1$b0>dat1$b0.low & dat1$b0<dat1$b0.high,1,0)
dat1$b1.cp <- ifelse(dat1$b1>dat1$b1.low & dat1$b1<dat1$b1.high,1,0)
dat1$a.cp <- ifelse(dat1$a>dat1$a.low & dat1$a<dat1$a.high,1,0)
dat1$ph.cp <- ifelse(dat1$ph>dat1$ph.low & dat1$ph<dat1$ph.high,1,0)

cp <- c(sum(dat1$b0.cp)/I,sum(dat1$b1.cp)/I,sum(dat1$a.cp)/I,sum(dat1$ph.cp)/I)

cbind(round(bias,3),round(mse,3),round(cp,3))

