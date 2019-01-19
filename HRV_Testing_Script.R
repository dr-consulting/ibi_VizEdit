library(psd)
library(imputeTS)
library(seewave)
IBI2<-read.table('file:///C:/Users/Mbars/Box Sync/Child and Family Relationships Lab/HRV_Rubin Editing/Editors/RA_Assignments/015_T2_child_Output/015_T2_child_Clown_IBI_edited.txt', 
                 header=TRUE)

Vid1_ts<-na.interpolation(DF_new$IBI, option='spline')
IBI_filter<-bwfilter(test.ts, from = .24, to = 1.04, bandpass = TRUE)

temp<-mvspec(x=IBI_filter,
             spans = c(7,7),
             taper = .01)

plot(temp$spec[temp$freq<.0015]~I(temp$freq[temp$freq<.0015]*1000),xlab="frequency",ylab="spectral density",type="l")


tot_power<-sum(temp$spec[temp$freq>=.00024 & temp$freq<=.00104])

#What about a different section of data - something we know should be lower in HRV terms. 
IBI2<-read.table('file:///C:/Users/Mbars/Box Sync/Child and Family Relationships Lab/HRV_Rubin Editing/Editors/RA_Assignments/015_T2_child_Output/015_T2_child_Clown_IBI_edited.txt', 
                 header=TRUE)

DF_new2<-data.frame(Time = seq(min(IBI2$Time), max(IBI2$Time), by=.001))
DF_new2<-merge(DF_new2, IBI2, by='Time', all=TRUE)

Clown_ts<-na.interpolation(DF_new2$IBI, option='spline')
Clown_ts<-ts(Clown_ts, frequency = 1000)
Clown_filter<-bwfilter(Clown_ts, from = .24, to = 1.04, bandpass = TRUE)
plot(Clown_filter)

temp<-mvspec(x=Clown_filter,
             spans = c(7,7),
             taper = .01)
plot(temp$spec[temp$freq<.0015]~I(temp$freq[temp$freq<.0015]*1000),xlab="frequency",ylab="spectral density",type="l")
tot_power<-sum(temp$spec[temp$freq>=.00024 & temp$freq<=.00104])
