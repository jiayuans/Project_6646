#setwd("C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/Result/Simulation_output")
setwd("C:/Users/jiayu/OneDrive/Desktop/BIOS6646")

###########################################################################
# Read csv files
text <- list.files(pattern="result.")
text1 <- unlist(lapply(strsplit(text,'.',fixed=TRUE),function(x) x[[1]]))
ind <- which(text1=="result")
num <- as.numeric(unlist(lapply(strsplit(text[ind],'.',fixed=TRUE),function(x) x[[2]])))

data_frames <- lapply(num, function(i) {
  file_name <- paste0("result.", i, ".csv") 
  read.csv(file_name)
})

I=length(data_frames)

Flag<-rep(NA,I)
B1.mean<-rep(NA,I)
B2.mean<-rep(NA,I)
B3.mean<-rep(NA,I)
cp1.mean<-rep(NA,I)
cp2.mean<-rep(NA,I)
c0.mean<-rep(NA,I)
c1.mean<-rep(NA,I)
c2.mean<-rep(NA,I)
c3.mean<-rep(NA,I)
c4.mean<-rep(NA,I)

u.tau.inv.mean<-rep(NA,I)
b0.mean<-rep(NA,I)
b1.mean<-rep(NA,I)
a.mean<-rep(NA,I)
ga.mean<-rep(NA,I)
w.tau.inv.mean<-rep(NA,I)
u.mean<-rep(NA,I)
v.mean<-rep(NA,I)
w.mean<-rep(NA,I)
eta.mean<-rep(NA,I)
eps.tau.inv.mean<-rep(NA,I)

for(i in 1:I){ 
Flag[i] <- ifelse(max(data_frames[[i]][,12])<2,1,0)
B1.mean[i] <- data_frames[[i]][1,5] 
B2.mean[i] <- data_frames[[i]][2,5] 
B3.mean[i] <-data_frames[[i]][3,5] 
cp1.mean[i] <-data_frames[[i]][4,5] 
cp2.mean[i] <-data_frames[[i]][5,5] 
c0.mean[i] <-data_frames[[i]][6,5] 
c1.mean[i] <-data_frames[[i]][7,5] 
c2.mean[i] <-data_frames[[i]][8,5] 
c3.mean[i] <-data_frames[[i]][9,5]
c4.mean[i] <-data_frames[[i]][10,5]

u.tau.inv.mean[i] <-data_frames[[i]][11,5] 
b0.mean[i] <-data_frames[[i]][12,5] 
b1.mean[i] <-data_frames[[i]][13,5] 
a.mean[i] <-data_frames[[i]][14,5] 
ga.mean[i] <-data_frames[[i]][15,5] 
w.tau.inv.mean[i] <-data_frames[[i]][16,5] 
eta.mean[i] <-data_frames[[i]][17,5] 
eps.tau.inv.mean[i] <-data_frames[[i]][19,5]

u.mean[i] <-mean(data_frames[[i]][820:1219,5])
v.mean[i] <-mean(data_frames[[i]][1220:1619,5])
w.mean[i] <-mean(data_frames[[i]][1620:2019,5])
 
}

Sim.results=cbind(Flag,B1.mean,B2.mean,B3.mean,cp1.mean,cp2.mean,c0.mean,c1.mean,c2.mean,c3.mean,c4.mean,u.tau.inv.mean,
               b0.mean,b1.mean,a.mean,ga.mean,w.tau.inv.mean,eta.mean,eps.tau.inv.mean,u.mean,v.mean,w.mean)
table(Flag)
Sim.results.1 <- subset(Sim.results,Flag==1)
round(colMeans(Sim.results.1),2)
round(colMeans(Sim.results),2)

#x10 <- round(colMeans(Sim.results.1),2)
#x <- round(colMeans(Sim.results),2)
#dat <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x)
#write.csv(dat, "resultall.csv")
