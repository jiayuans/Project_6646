dirg <- "C:/UCHealth/RA/Project/EPIC-CF/Analysis_Jiayuan/EPIC/"
setwd(dirg)
##################################################################
##    Functions to Read data
## 
##################################################################
##dat.pa <- read.csv(file=paste(dirg,"/Data-PA-cohort.csv",sep=""))
##dat.pa <- dat.pa[,-1]

##dat.pa1 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
##                 FUN=max, na.rm=TRUE)
##names(dat.pa1) <- c('cffidno','age.max')
##
##dat.pa2 <-aggregate(dat.pa$age.min, by=list(dat.pa$cffidno),
##                 FUN=max, na.rm=TRUE)
##names(dat.pa2) <- c('cffidno','age.min')

##first.t<-dat.pa2$age.min
##last.t<-dat.pa1$age.max
##first.tt<-first.t[c(101:300,1001:1200)]
##last.tt<-last.t[c(101:300,1001:1200)]

##tt <- cbind(first.tt,last.tt)
##write.csv(tt,"long.time.csv")

dat.pa <- read.csv(file=paste("C:/UCHealth/RA/Project/EPIC-CF/Data/Source","/Data-PA-cohort.csv",sep=""))
dat.pa <- dat.pa[,-1]

sexf <- dat.pa[!duplicated(dat.pa$cffidno,dat.pa$sexf),5]

dat.pa1 <-aggregate(dat.pa$VisitAge, by=list(dat.pa$cffidno),
                    FUN=max, na.rm=TRUE)
names(dat.pa1) <- c('cffidno','age.max')

dat.pa2 <-aggregate(dat.pa$age.min, by=list(dat.pa$cffidno),
                    FUN=max, na.rm=TRUE)
names(dat.pa2) <- c('cffidno','age.min')

first.t<-dat.pa2$age.min
last.t<-dat.pa1$age.max
first.tt<-first.t[c(1:401)]
last.tt<-last.t[c(1:401)]
sexf<-sexf[c(1:401)]

first.tt<-first.tt[-359]
last.tt<-last.tt[-359]
sexf<-sexf[-359]

lt1 <- cbind(first.tt,last.tt)
lt2 <- cbind(first.tt,last.tt,sexf)
write.csv(lt1,"long.data.csv")
write.csv(lt2,"long.data1.csv")
