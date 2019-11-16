# Import and Process

generate_ibis_w_timing <- function(clean_ppg, ds, Hz, ppg_col="PPG", time_col="Time"){
  # Generating IBIs:
  IBI_list <- iter_IBI(clean_ppg[ppg_col], ds=1000)
  IBI <- IBI_list$IBI_done
  IBI_time <- sum_rev(IBI)
  IBI_matrix <- cbind(IBI, IBI_time+min(clean_ppg[time_col]))
  colnames(IBI_matrix)<-c('IBI', 'Time')

  # Saving reactive values
  # If this does not work will have to pull this out as a separate step after function runs
  rv$IBI_raw <- as.data.frame(IBI_file)

  # Come back and find a way to create a range of values that results in some large portion of values within a 0-2 range.
  # Will make visual editing easier down the road.
  rv$PPG_proc$Time <- rv$PPG_proc$Time - min(rv$sub_time$Time, na.rm = TRUE)
  rv$mean_PPG_proc <- mean(rv$PPG_proc[ppg_col], na.rm = TRUE)
  rv$PPG_proc$PPG<-(rv$PPG.proc$PPG - rv$mean.PPG.proc)+mean(rv$IBI.raw$IBI)

  # I do need to make sure that I am architecting this in a way that lets me parallel track these.
  rv$PPG_1000$Time <- rv$PPG_1000$Time - min(rv$sub_time$Time, na.rm=T)
  rv$mean_PPG1000<-mean(rv$PPG_1000$PPG, na.rm = T)
  rv$PPG_1000$PPG<-(rv$PPG_1000$PPG-rv$mean_PPG1000)+mean(rv$IBI_raw$IBI)

  # Maybe the best way to deal with this is to have rv as an input and output of the function?
  # Note need to address the fact that the ds value should always be lower than the Hz value.
  rv$PPG_ds_editable <- rv$PPG_1000
  rv$PPG_ds_fixed <- rv$PPG_1000
  rv$PPG_GP_fixed <- data.frame(PPG=rep(NA, nrow(rv$PPG_1000)),
                                Time=rv$PPG_1000$Time)

  # Now making the editable IBI dataframe
  rv$IBI_edited <- rv$IBI_raw

  # Stopped here. I may want to re-thin this entire approach.
  # Probably worthwhile to break these out into smaller programs and then run them all inside the relevant observe event
  # In fact that should be the logic. Run a set of smaller functions within the reactive event.



}







observeEvent(input$submit.file,{
  #PPG data import, processing and saving
  if(!is.null(input$submit.file)){
    #browser()
    PPG.cln<-rv$PPG.1000
    #-----------------------------------------------------------
    #IBI data processing and saving
    ds<-DS()
    Hz<-Hz()
    IBI.list<-iter.IBI(PPG.cln$PPG, ds=1000)
    IBI<-IBI.list$IBI.done
    IBI.time<-sum.rev(IBI)
    IBI.file<-cbind(IBI, IBI.time+min(PPG.cln$Time))
    colnames(IBI.file)<-c('IBI', 'Time')
    rv$IBI.raw<-as.data.frame(IBI.file)
    rv$PPG.proc$Time<-rv$PPG.proc$Time-min(rv$sub.time$Time, na.rm=T)
    rv$mean.PPG.proc<-mean(rv$PPG.proc$PPG, na.rm = T)
    rv$PPG.proc$PPG<-(rv$PPG.proc$PPG-rv$mean.PPG.proc)+mean(rv$IBI.raw$IBI)
    rv$PPG.1000$Time<-rv$PPG.1000$Time-min(rv$sub.time$Time, na.rm=T)
    rv$mean.PPG1000<-mean(rv$PPG.1000$PPG, na.rm = T)
    rv$PPG.1000$PPG<-(rv$PPG.1000$PPG-rv$mean.PPG1000)+mean(rv$IBI.raw$IBI)
    sel.vals<-seq(1, length(rv$PPG.1000$Time), by=10)
    rv$PPG.proc2<-rv$PPG.1000[sel.vals,]
    rv$PPG.proc2$Vals<-rep('original', length(rv$PPG.proc2[,1]))
    rv$PPG.100<-rv$PPG.proc2
    rv$PPG.GP<-data.frame(PPG=rep(NA, length(rv$PPG.proc2$PPG)),
                          Time=rv$PPG.proc2$Time)

    rv$IBI.edit<-as.data.frame(IBI.file)
    rv$IBI.edit$Time<-rv$IBI.edit$Time-min(rv$sub.time$Time, na.rm=T)
    rv$IBI.edit2<-rv$IBI.edit
    rv$IBI.edit$Vals<-rep('Original', length(rv$IBI.edit[,1]))
    rv$sub.time$Time<-rv$sub.time$Time-min(rv$sub.time$Time, na.rm=T)
    rv$tab<-head(as.data.frame(IBI.list$Z))
    rv$tab.comp<-as.data.frame(IBI.list$Z)
  }
})
