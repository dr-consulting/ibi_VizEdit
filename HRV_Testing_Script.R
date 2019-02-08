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
IBI2<-read.table('C:/Users/Mbars/Box Sync/Child and Family Relationships Lab/HRV_Rubin Editing/Administrators/HRV_Testing/002_T2/002_T2__Video2_IBI_edited.txt', 
                 header=TRUE)

DF_new2<-data.frame(Time = seq(min(IBI2$Time), max(IBI2$Time), by=.001))
DF_new2<-merge(DF_new2, IBI2, by='Time', all=TRUE)

Clown_ts<-na.interpolation(DF_new2$IBI, option='spline')
Clown_ts<-ts(Clown_ts, frequency = 1000)
Clown_filter<-bwfilter(Clown_ts, from = .24, to = 1.04, bandpass = TRUE, f=1000)

plot(Clown_filter)
plot(temp$spec[temp$freq<.0015]~I(temp$freq[temp$freq<.0015]*1000),xlab="frequency",ylab="spectral density",type="l")
tot_power4<-sum(temp$spec[temp$freq>=.00024 & temp$freq<=.00104])


start.vals<-seq(1, 160001, by=10000)
stop.vals<-seq(10000, 170000, by=10000)

spec_10secs<-vector()
for(i in 1:length(start.vals)){
  temp<-Clown_filter[start.vals[i]:stop.vals[i]]
  temp.spec<-mvspec(x=Clown_filter[,1],
                    spans = c(7,7),
                    taper = .1,
                    plot=FALSE)
  tot_power4<-sum(temp.spec$spec[temp.spec$freq>=.00024 & temp.spec$freq<=.00104])
}


tot_power2


#Let's try adding this to final results as separate set of variability functions: 
#Can get low frequency 

#Attempting my approach - goal is 
wd<-'C:/Users/Mbars/Box Sync/Child and Family Relationships Lab/HRV_Rubin Editing/Administrators/HRV_Testing'
dirs<-list.dirs(wd)[-1]

IDs<-vector()
Task<-vector()
HF_HRV<-vector()
ln_HF_HRV<-vector()

for(d in 1:length(dirs)){
  #d<-1
  temp.wd<-dirs[d]
  files<-list.files(temp.wd)
  setwd(temp.wd)
  for(f in 1:length(files)){
    #f<-1
    IBI.temp<-read.table(files[f], header = TRUE)
    DF.temp<-data.frame(Time = seq(min(IBI.temp$Time), max(IBI.temp$Time), by=.001))
    DF.temp<-merge(DF.temp, IBI.temp, by='Time', all=TRUE)
    IBI.ts<-na.interpolation(DF.temp$IBI, option='spline')
    IBI.ts<-ts(IBI.ts, frequency = 1000)
    IBI_filter<-bwfilter(IBI.ts, from = .24, to =1.04, bandpass = TRUE)
    temp.spec<-mvspec(x=IBI_filter,
                      spans = c(13,13),
                      taper = .1, 
                      plot = FALSE)
    ID.temp<-substr(files[f], start = 1, stop=6)
    png(paste0(temp.wd, '/', ID.temp, strsplit(files[f], split = '_')[[1]][4], '.png'), 
        res=600, 
        units = 'in', 
        width = 6, 
        height = 6)
    plot(temp.spec$spec[temp.spec$freq<.0015]~I(temp.spec$freq[temp.spec$freq<.0015]*1000),
         xlab="frequency",ylab="spectral density",type="l", 
         main = paste(ID.temp, 
                      strsplit(files[f], split = '_')[[1]][4]))
    dev.off()
    IDs<-c(IDs, ID.temp)
    Task<-c(Task, strsplit(files[f], split='_')[[1]][4])
    HF_HRV<-c(HF_HRV, sum(temp.spec$spec[temp.spec$freq>=.00024 & temp.spec$freq<=.00104]))
    ln_HF_HRV<-c(ln_HF_HRV, log(sum(temp.spec$spec[temp.spec$freq>=.00024 & temp.spec$freq<=.00104])))
  }
}

MB_DF<-data.frame(ID = IDs, 
                  Task = Task, 
                  HF_HRV = HF_HRV, 
                  ln_HF_HRV = ln_HF_HRV, 
                  stringsAsFactors = FALSE)
table(MB_DF$Task)

CE_DF<-read.csv(paste0(wd, '/CE_RSA.csv'), 
                stringsAsFactors = FALSE)

colnames(CE_DF)[1]<-'ID'
df.m<-reshape2::melt(CE_DF, id.var='ID')

colnames(df.m)[2:3]<-c('Task', 'CB_RSA')
df.m$Task<-as.character(df.m$Task)

df.m$Task[df.m$Task=='RSA15_Video1']<-'Video1'
df.m$Task[df.m$Task=='RSA15_Video2']<-'Video2'
df.m$Task[df.m$Task=='RSA15_Video3']<-'Video3'
df.m$Task[df.m$Task=='RSA15_Clown']<-'Clown'
df.m$Task[df.m$Task=='RSA15_Intro']<-'Introduction'
df.m$Task[df.m$Task=='RSA15_Kids']<-'Kids'


DF_comb<-merge(MB_DF, df.m, by=c('ID', 'Task'))
psych::pairs.panels(DF_comb[,3:5])
#Trying to understand these plots more: 
library(ggplot2)

g1<-ggplot(data = DF_comb, 
           aes(x=ln_HF_HRV, 
               y=CB_RSA))+
  geom_point()+
  stat_smooth(method='lm')+
  geom_text(aes(label = paste(ID, Task)))
g1
