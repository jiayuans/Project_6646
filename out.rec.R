setwd("C:/Users/jiayu/OneDrive/Desktop/BIOS6646")

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
#Flag  b0.mean  b1.mean   a.mean  ph.mean ga0.mean ga1.mean   v.mean 
#1.00    -1.19     0.21     1.67     0.63     0.94     0.90     1.00 
#> round(colMeans(Sim.results),2)
#Flag  b0.mean  b1.mean   a.mean  ph.mean ga0.mean ga1.mean   v.mean 
#0.03    -1.09     0.10     1.83     4.40     1.18     0.80     3.38 