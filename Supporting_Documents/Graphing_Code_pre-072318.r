  observeEvent(input$submit.zoom, {
    rv$y.axis.min<-input$y.axis[1]
    rv$y.axis.max<-input$y.axis[2]
  })
  
  output$IBI <- renderPlot({
    #browser()
    if(is.null(rv$IBI.edit)){
      temp.df<-data.frame(x=c(-1,0,1), y=c(-1,0,1))
      p.IBI<-ggplot(aes(x=x, y=y), data=temp.df)+
        annotate('text', x=0, y=0, label='No Processed Data Provided')
    }
    else if(is.null(input$zoom_brush)){
      p.IBI<-ggplot(data = rv$IBI.edit, aes(x=Time, y=IBI))+
        geom_point(col="red")+
        geom_line(col="black")+
        xlab('Time(s)')+
        ylab('IBI(s)')
      
      if(length(rv$IBI.edit$Vals[rv$IBI.edit$Vals=='Uneditable'])>0){
        p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=rv$IBI.edit[rv$IBI.edit$Vals=='Uneditable',], color='#58D3F7')
      }
        
      if(!is.null(rv$sub.time)){
        p.IBI<-p.IBI+geom_vline(aes(xintercept=Time, color=Task), data=rv$sub.time, show.legend = F)+
          geom_text(aes(x=Time, label=Label, color=Task, y=.25), data = rv$sub.time, show.legend = F,
                    angle = 60, hjust=0)
      }
    }
      
    else if(!is.null(input$zoom_brush)){
      time.min<-as.numeric(input$zoom_brush$xmin)
      time.max<-as.numeric(input$zoom_brush$xmax)
      IBI.tmp<-rv$IBI.edit[rv$IBI.edit$Time>=time.min & rv$IBI.edit$Time<=time.max,]
      PPG.tmp<-rv$PPG.1000[rv$PPG.1000$Time>=time.min & rv$PPG.1000$Time<=time.max,]
      
      p.IBI<-ggplot(data = IBI.tmp, aes(x=Time, y=IBI))+
        geom_point(col="red")+
        geom_line(col="black")+
        xlab('Time(s)')+
        ylab('IBI(s)')+
        geom_vline(aes(xintercept=Time), data=IBI.tmp, color = 'red', lty='dashed', alpha=.25)+
        geom_line(aes(x=Time, y=PPG), data=PPG.tmp, col='gray80')+
        scale_y_continuous(limits = c(rv$y.axis.min, rv$y.axis.max))
      
      if(!is.null(rv$sub.time)){
        sub.time.tmp<-rv$sub.time[rv$sub.time$Time>=time.min & rv$sub.time$Time<=time.max,]
        if(nrow(sub.time.tmp)>0){
          p.IBI<-p.IBI+geom_vline(aes(xintercept=Time, color=Task), data=sub.time.tmp, show.legend = F)+
            geom_text(aes(x=Time, label=Label, color=Task, y=.25), data = sub.time.tmp, show.legend = F,
                      angle = 60, hjust=0)
        }
      }
        
      if(!is.null(input$select_cases)){
        IBI.temp<-brushedPoints(df=IBI.tmp, input$select_cases)
        p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=IBI.temp, col='#82FA58')
        if(length(IBI.tmp$Vals[IBI.tmp$Vals=='Uneditable'])>0){
          p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=IBI.tmp[IBI.tmp$Vals=='Uneditable',], color='#58D3F7')
        }
      }
        
      if(!is.null(rv$PPG.GP) & length(na.omit(rv$PPG.GP[,1]))>0){
        PPG.GP.tmp<-rv$PPG.GP[rv$PPG.GP$Time>=time.min & rv$PPG.GP$Time<=time.max,]
        if(length(na.omit(PPG.GP.tmp$PPG))>0){
          p.IBI<-p.IBI+geom_line(aes(x=Time, y=PPG), 
                                 data = PPG.GP.tmp,
                                 col='#58D3F7')
        }
        else{
          p.IBI<-p.IBI
        }
      }
    }
    p.IBI
  })
  
  output$IBI2 <- renderPlot({
    #browser()
    if(is.null(rv$IBI.edit)){
      temp.df<-data.frame(x=c(-1,0,1), y=c(-1,0,1))
      p.IBI<-ggplot(aes(x=x, y=y), data=temp.df)+
        annotate('text', x=0, y=0, label='No Processed Data Provided')
    }
    else if(is.null(input$zoom_brush)){
      p.IBI<-ggplot(data = rv$IBI.edit, aes(x=Time, y=IBI))+
        geom_point(col="red")+
        geom_line(col="black")+
        xlab('Time(s)')+
        ylab('IBI(s)')
      
      if(length(rv$IBI.edit$Vals[rv$IBI.edit$Vals=='Uneditable'])>0){
        p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=rv$IBI.edit[rv$IBI.edit$Vals=='Uneditable',], color='#58D3F7')
      }
      
      if(!is.null(rv$sub.time)){
        p.IBI<-p.IBI+geom_vline(aes(xintercept=Time, color=Task), data=rv$sub.time, show.legend = F)+
          geom_text(aes(x=Time, label=Label, color=Task, y=.25), data = rv$sub.time, show.legend = F,
                    angle = 60, hjust=0)
      }
    }
    
    else if(!is.null(input$zoom_brush)){
      time.min<-as.numeric(input$zoom_brush$xmin)
      time.max<-as.numeric(input$zoom_brush$xmax)
      IBI.tmp<-rv$IBI.edit[rv$IBI.edit$Time>=time.min & rv$IBI.edit$Time<=time.max,]
      PPG.tmp<-rv$PPG.proc2[rv$PPG.proc2$Time>=time.min & rv$PPG.proc2$Time<=time.max,]
      
      p.IBI<-ggplot(data = IBI.tmp, aes(x=Time, y=IBI))+
        geom_point(col="red")+
        geom_line(col="black")+
        xlab('Time(s)')+
        ylab('IBI(s)')+
        geom_vline(aes(xintercept=Time), data=IBI.tmp, color = 'red', lty='dashed', alpha=.25)+
        geom_line(aes(x=Time, y=PPG), data=PPG.tmp, col='gray80')+
        scale_y_continuous(limits = c(rv$y.axis.min, rv$y.axis.max))
      
      if(length(IBI.tmp$Vals[IBI.tmp$Vals=='Uneditable'])>0){
        p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=IBI.tmp[IBI.tmp$Vals=='Uneditable',], color='#58D3F7')
      }
      
      if(!is.null(rv$sub.time)){
        sub.time.tmp<-rv$sub.time[rv$sub.time$Time>=time.min & rv$sub.time$Time<=time.max,]
        if(nrow(sub.time.tmp)>0){
          p.IBI<-p.IBI+geom_vline(aes(xintercept=Time, color=Task), data=sub.time.tmp, show.legend = F)+
            geom_text(aes(x=Time, label=Label, color=Task, y=.25), data = sub.time.tmp, show.legend = F,
                      angle = 60, hjust=0)
        }
      }
      
      if(!is.null(input$select_cases2)){
        IBI.temp<-brushedPoints(df=IBI.tmp, input$select_cases2)
        p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=IBI.temp, col='#82FA58')
      }
      
      if(!is.null(rv$PPG.GP) & length(na.omit(rv$PPG.GP[,1]))>0){
        PPG.GP.tmp<-rv$PPG.GP[rv$PPG.GP$Time>=time.min & rv$PPG.GP$Time<=time.max,]
        if(length(na.omit(PPG.GP.tmp$PPG))>0){
          p.IBI<-p.IBI+geom_line(aes(x=Time, y=PPG), 
                                 data = PPG.GP.tmp,
                                 col='#58D3F7')
        }
        else{
          p.IBI<-p.IBI
        }
      }
    }
    p.IBI
  })
  
  output$PPG_overall<-renderPlot({
    #browser()
    if(is.null(rv$PPG.proc)){
      temp.df<-data.frame(x=c(-1,0,1), y=c(-1,0,1))
      p.PPG2<-ggplot(aes(x=x, y=y), data=temp.df)+
        annotate('text', x=0, y=0, label='No Data Provided')
    }
    else{
      p.PPG2<-ggplot()+
        geom_line(aes(x=Time, y=PPG), data=rv$PPG.proc, color='black')+
        ylab('Volts')+xlab('Time(s)')  
    }
    p.PPG2
  })
  
  output$hover_info <- renderPrint({
    temp.points<-nearPoints(df=rv$IBI.edit[,1:2], coordinfo = input$plot_hover, maxpoints = 1)
    mean.HR<-NA
    if(!is.null(rv$IBI.edit)){
      mean.HR<-1/mean(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'][5:length(rv$IBI.edit$IBI)-5])*60
      names(mean.HR)<-'Est. Mean HR'
    }
    if(!is.null(input$plot_hover)){
      cat("Plot Values:\n")
      mean.HR
      round(temp.points, digits = 3)
    }
    else{
      cat("Plot Values:\n")
      mean.HR
    }
  })
  
  output$hover_info2 <- renderPrint({
    temp.points<-nearPoints(df=rv$IBI.edit[,1:2], coordinfo = input$plot_hover2, maxpoints = 1)
    mean.HR<-NA
    if(!is.null(rv$IBI.edit)){
      mean.HR<-1/mean(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'][5:length(rv$IBI.edit$IBI)-5])*60
      names(mean.HR)<-'Est. Mean HR'
    }
    if(!is.null(input$plot_hover2)){
      cat("Plot Values:\n")
      mean.HR
      round(temp.points, digits=3)
    }
    else{
      cat("Plot Values:\n")
      mean.HR
    }
  })