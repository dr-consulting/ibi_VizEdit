library(shiny)

#' Shiny reactive object
#'
#' \code{generate_ppg_data_check_plot} defines the properties of the plot called on the UI Processing Panel to check
#' that the PPG data loading process was successful. The utility and resulting plot allow a user to visually inspect
#' the imported signal based on settings defined on the UI Data Entry panel
#'

generate_ppg_data_check_plot <- reactive({
  p <- ggplot()
  if(!is.null(input$fileIn)){
    PPG_plot<-as.data.frame(rv$PPG_proc)
    PPG_plot <- PPG.plot[PPG.plot$Time > view.min() & PPG.plot$Time < view.max(),]

    p <- p + geom_line(aes(x=Time, y=PPG), data=PPG.plot, color='black')+
      ylab('PPG (Volts)')+
      xlab('Time (s)')
  }
  else{
    text <- 'Either load data or select "View Plot"'
    p <- p+annotate("text", x = 4, y = 25, size=8, label = text)
  }
  p
})

#' Shiny reactive object
#'
#' \code{generate_base_gui_plot} defines the base plot that forms the basis for interactively editing IBIs
#'

generate_base_gui_plot <- reactive({
  if(is.null(rv$IBI_user_edited)){
    p <- ggplot(data=data.frame(x=c(-1, 0, 1),
                                y=c(-1, 0, 1)),
                aes(x=x, y=y))+
      annotate('text', x=0, y=0, label='No Processed Data Provided')
  }
  else if(!is.null(rv$IBI_user_edited)){
      p <- ggplot(data=rv$IBI_user_edited,
                  aes(x=Time, y=IBI))+
        geom_point(color="red")+
        geom_line(color="black")+
        labs(x="Time(s)", y="IBI(s)")
  }
  p
})



output$IBI <- renderPlot({
  #browser()
  if(is.null(rv$IBI.edit)){
    temp.df<-data.frame(x=c(-1,0,1), y=c(-1,0,1))
    p.IBI<-ggplot(aes(x=x, y=y), data=temp.df)+
      annotate('text', x=0, y=0, label='No Processed Data Provided')
  }
  else if(!is.null(rv$IBI.edit)){
    if(is.null(input$zoom_brush)){
      p.IBI<-ggplot(data = rv$IBI.edit, aes(x=Time, y=IBI))+
        geom_point(col="red")+
        geom_line(col="black")+
        xlab('Time(s)')+
        ylab('IBI(s)')
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
        scale_y_continuous(limits = c(rv$y.axis.min, rv$y.axis.max))+
        scale_x_continuous(limits = c(time.min, time.max))

      if(!is.null(rv$sub.time)){
        p.IBI<-p.IBI+geom_vline(aes(xintercept=Time, color=Task), data=rv$sub.time, show.legend = F)+
          geom_text(aes(x=Time, label=Label, color=Task, y=.25), data = rv$sub.time, show.legend = F,
                    angle = 60, hjust=0)
      }

      if(rv$ppg.on==1){
        p.IBI<-p.IBI+geom_line(aes(x=Time, y=PPG), data=PPG.tmp, col='gray')
      }
    }

    if(length(rv$IBI.edit$Vals[rv$IBI.edit$Vals=='Uneditable'])>0){
      p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=rv$IBI.edit[rv$IBI.edit$Vals=='Uneditable',], color='#58D3F7')
    }
    edit.pnts<-rv$IBI.edit[rv$IBI.edit$Vals!='Uneditable',]
    edit.pnts<-edit.pnts[edit.pnts$Vals!='Original',]
    if(length(edit.pnts[,1])>0){
      p.IBI<-p.IBI+geom_point(data=edit.pnts, aes(x=Time, y=IBI), color='#bb8fce')
    }
    if(!is.null(input$select_cases)){
      IBI.temp<-brushedPoints(df=rv$IBI.edit, input$select_cases)
      p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=IBI.temp, col='#82FA58')
    }
  }
  p.IBI
})
