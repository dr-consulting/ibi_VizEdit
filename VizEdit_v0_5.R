#===================================================================================================
# This is the Shiny web application IBI VizEdit - Matthew G. Barstead (c) 2017. 
# You can run the application by clicking the 'Run App' button above.
#
# By running this application you agree to the terms outlined at the link below:
# [insert weblink]
#
# Details about the processing steps are detailed at the link below: 
# [insert weblink]
#
# Please cite the use of IBI VizEdit according to standard practices in your field when publishing
# (see output for citation details)
#
# General questions? Contact the developer Matthew G. Barstead 
# Contact: barstead@umd.edu
#===================================================================================================

require(pacman)
pacman::p_load(shiny, 
               ggplot2, 
               shinythemes,
               shinyFiles,
               signal,
               zoo,
               forecast,
               psych,
               rtf, 
               shinyBS, 
               rpart, 
               party,
               tseries, 
               seewave)

library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyFiles)
library(signal)
library(zoo)
library(forecast)
library(rtf)
library(shinyBS)
library(rpart)
library(party)
library(tseries)


# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(theme = shinytheme('united'),
  titlePanel(
    'IBI VizEdit v0.5'
    ),
  tabsetPanel(
    tabPanel('Data Entry',
#############################################
      wellPanel(fluidRow(
        column(3,
               tags$h2('Select File and Directory:'
                       ),
               shinyDirButton(id='dir',
                              label='Select Directory',
                              title = 'Choose Your Working Directory'
                              ),
               tags$br(),
               tags$p(textOutput('dir.out')
                      ),
               tags$br(),
               shinyFilesButton(id='fileIn', 
                                title = 'Choose Heart Rate File:', 
                                label = 'Select HR File', 
                                multiple = F
                                ), 
               tags$br(),
               tags$p(textOutput(outputId = 'name'))
               ),
        column(3,
               tags$h2('File ID and Information:'
                       ),
               textInput(inputId='sub.id', 
                         label = 'Subject ID:'
                         ),
               textInput(inputId='study.id',
                         label = 'Study ID:'
                         ),
               textInput(inputId='time.id',
                         label = 'Time Point:'
                         ), 
               textInput(inputID='editor',
                         label = 'Editor Name:'
                         )
               ),
        column(3,
               tags$h2('Data Details:'
                       ),
               numericInput(inputId='col.select', 
                            label = 'Data is in column:',
                            min = 0, 
                            max = 999, 
                            value = 2
                            ),
               numericInput(inputId='header', 
                            label = 'Number of header lines in file:',
                            min = 0, 
                            max = 200, 
                            value = 15
                            ),
               numericInput(inputId='Hz',
                            label = 'File Sampling Rate in Hz',
                            min = 125, 
                            max = 2000, 
                            value = 2000
                            ),
               numericInput(inputId='DS',
                            label = 'Preferred Downsampling Rate',
                            min = 25,
                            max = 2000,
                            value = 100
                            ),
               tags$p('Note: Larger values increase computation time')
               ), 
        column(3,
               tags$h2('Optional Settings:'
                       ),
               shinyFilesButton(id='timeIn', 
                                title = 'Optional Timing File:', 
                                label = 'Select Timing File', 
                                multiple = F
                                ),
               textOutput(outputId = 'time.out'
                          ),
               tags$br(),
               numericInput(inputId='peak.iter',
                            label = 'Peak Detection Iterations',
                            min = 10, 
                            max = 50, 
                            value = 25
                            ),
               tags$div(checkboxGroupInput(inputId='epoch.in',
                                           label = 'Output Epoch Options:',
                                           choiceNames = c('10s', '15s', '20s', '30s', '45s'),
                                           choiceValues = c('10', '15', '20', '30', '45'), 
                                           selected = c('15'),
                                           inline = F)
                        )
               )
        )
      ),
      wellPanel(fluidRow(
        column(3,
               tags$h4('Load file using current settings'
                       ),
               tags$button(id='load', 
                           type = "button",
                           class = "btn action-button",
                           style="color: #000000; background-color: #82FA58; border-color: #FFFFFF", 
                           'Load File Settings'
                           )
               ),
        column(3,
               tags$h4('Reset fields to change existing file'
                       ),
               tags$button(id='reset',
                           type = "button",
                           class = "btn action-button",
                           style="color: #000000; background-color: #FA8258; border-color: #FFFFFF", 
                           'Reset File Settings'
                           )
               ),
        column(3,
               tags$h4('Save Edited File'
               ),
               tags$button(id = 'save',
                           type = "button",
                           class = "btn action-button",
                           style = "color: #000000; background-color: #82FA58; border-color: #FFFFFF",
                           'Save'
                           )
               ),
        column(3,
               tags$h4('Save Edited File and Close'
               ),
               tags$button(id = 'save.close',
                           type = "button",
                           class = "btn action-button",
                           style = "color: #000000; background-color: #FA8258; border-color: #FFFFFF",
                           onclick = "setTimeout(function(){window.close();},4000);",  # close browser
                           "Save & Close window"
                           )
        )
        )
        )
    ),
    #Tab2 - Data Pre-Flight
    tabPanel('Processing Panel',
    ####################################
      wellPanel(fluidRow(
        column(6,
               tags$h3('View plot of heart rate signal'
                       ),
               tags$button(id='view.ppg',
                           type = "button",
                           class = "btn action-button",
                           style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF", 
                           'View HR Plot'
                           ),
               tags$p('Use slider to adjust plot window width'
                      ),
               sliderInput(inputId='Viewer', 
                           min = 0, 
                           max = 1200, 
                           value = c(0, 60), 
                           post = 'seconds',
                           label = 'Choose View Window'
                           ),
               plotOutput(outputId = 'PPG.check'
                          )
               ),
        column(3,
               tags$h3('Process Raw File'
                       ),
               tags$button(id='submit.file',
                           type = "button",
                           class = "btn action-button",
                           style="color: #000000; background-color: #82FA58; border-color: #FFFFFF", 
                           'Process File'
                           ),
               tags$p('Processing Optimization Information:'
                      ),
               tableOutput(outputId = 'Iteration.Data'
                           )
               ), 
        column(3,
               tags$h3('Event Summary Timing:'
                       ),
               tableOutput(outputId = 'Event.Data'
                           )
               )
        )
        )
    ),
    tabPanel('Basic Editing Panel',
       fluidRow(
         column(3,
                tags$hr(),
                tags$p('Toggle Base Functions:'),
                uiOutput(outputId = 'base.on'
                         ),
                tags$br(),
                tags$p('Select Function:'),
                uiOutput(outputId = 'add.on', 
                         inline = T
                         ),
                uiOutput(outputId = 'divide.on', 
                         inline = T
                         ),
                uiOutput(outputId = 'average.on', 
                         inline = T
                         ),
                numericInput(inputId = 'divide.by',
                             label = 'Divide by:', 
                             min = 2, 
                             max = 5, 
                             value = 2
                             ),
                tags$hr(),
                verbatimTextOutput("hover_info"),
                tags$hr(),
                tags$p('Editing Tools:'),
                uiOutput(outputId = 'add.delete.on', 
                         inline = T
                         ),
                uiOutput(outputId = 'select.on', 
                         inline = T
                         )
                ),
         column(9,
                plotOutput(outputId = "IBI",
                           height = '450px',
                           click = "Peak_click",
                           dblclick = "Delete",
                           brush = "select_cases",
                           hover = hoverOpts(id="plot_hover", delay = 500)
                           )
                ),
         column(9,
                plotOutput(outputId = "PPG_overall",
                           height = '150px',
                           hover = hoverOpts(id="plot_hover", delay = 500),
                           brush =  brushOpts(id="zoom_brush", direction = 'x')
                           )
                )
         )
       ),
    tabPanel('Advanced Editing Panel',
      fluidRow(
        column(3,
               tags$hr(),
               tags$p('Toggle Advanced Functions:'),
               uiOutput(outputId = 'adv.on', 
                        inline = T
                        ),
               tags$br(),
               tags$br(),
               uiOutput(outputId = 'sim.on', 
                        inline = T
                        ),
               uiOutput(outputId = 'predict.on', 
                        inline = T
                        ),
               tags$hr(),
               numericInput(inputId = 'n.sims',
                            label = 'Simulations',
                            value = 100, 
                            min = 10,
                            max = 10000
                            ),
               tags$hr(),
               sliderInput(inputId = 'freq.select',
                           label = 'Select Target HR',
                           min = 40, 
                           max = 140,
                           value = c(80, 120),
                           post = 'BPM'
                           ),
               tags$hr(),
               verbatimTextOutput(outputId = "hover_info2"
                                  ),
               tags$hr(),
               tags$p('Editing Tools:'),
               uiOutput(outputId = 'add.delete.on2', 
                        inline = T
                        ),
               uiOutput(outputId = 'select.on2', 
                        inline = T
                        )
               ),
        column(9,
               plotOutput(outputId = "IBI2",
                          height = '600px',
                          click = "Peak_click2",
                          dblclick = "Delete2",
                          brush = "select_cases",
                          hover = hoverOpts(id="plot_hover2", delay = 500)
                          )
               )
      )
)
)
)
)

server <- function(input, output) {
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Global Options for the Application:
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  options(shiny.maxRequestSize=150*1024^2)
  user.folder<-Sys.getenv('USERPROFILE') 
  rv<-reactiveValues(
    tot.edits=data.frame(),
    base.on=0,
    adv.on=0,
    pred.sim=NULL,
    select.on=0,
    add.delete.on=0,
    rf20=NULL,
    select.on2=0,
    add.delete.on2=0,
    start.time=NULL
  )
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Global Options for the Application:
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Note - want to program here so that the file structure follows working directory...
  shinyDirChoose(input, 'dir', roots=c(User=user.folder))
  observeEvent(input$dir,{
    if(!is.null(input$dir)){
      rv$wd<-parseDirPath(roots=c(User=user.folder), input$dir)
      shinyFileChoose(input, 'fileIn', roots=c(wd=rv$wd, User=user.folder))
      shinyFileChoose(input, 'timeIn', roots=c(wd=rv$wd, User=user.folder))
    }
    else{
      shinyFileChoose(input, 'fileIn', roots=c(User=user.folder))
      shinyFileChoose(input, 'timeIn', roots=c(User=user.folder))
    }
  })
  
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Text display for File Name and Working Directory
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  text.display1<-reactive({
    if(is.null(input$dir)){
      text1<-paste("Directory:", "WARNING - no directory selected!")
    }
    else{
      text1<-paste("Directory:", rv$out.dir) 
    }
    text1
  })
  
  output$dir.out<-renderText({
    text.display1()
  })
  
  text.display2<-reactive({
    if(is.null(input$fileIn)){
      text2<-paste("File Chosen:", "WARNING - no file selected!")
    }
    else{
      text2<-paste("File Chosen:", rv$file.name) 
    }
    text2
  })
  
  output$name<-renderText({
    text.display2()
  })
  
  text.display3<-reactive({
    if(is.null(input$timeIn)){
      text3<-paste("File Chosen:", "No file selected")
    }
    else{
      text3<-paste("File Chosen:", rv$time.name) 
    }
    text3
  })
  
  output$time.out<-renderText({
    text.display3()
  })
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Input information for subject/Editor
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  sub.id<-reactive({input$sub.id})
  study.id<-reactive({input$study.id})
  time.id<-reactive({input$time.id})
  editor.id<-reactive({input$editor})

  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Input information for data
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  Hz<-reactive({as.numeric(input$Hz)})
  DS<-reactive({as.numeric(input$DS)})
  skip.vals<-reactive({as.numeric(input$header)})
  col.num<-reactive({as.numeric(input$col.select)})
  epoch<-reactive({as.numeric(input$epoch.in)})
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Optional Peak Iteration Information
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  peak.iter<-reactive({as.numeric(input$peak.iter)})
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #View Values Peak Iteration Information
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  View.min<-reactive(as.numeric(input$Viewer[1]))
  View.max<-reactive(as.numeric(input$Viewer[2]))
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Hitting Load Button results in: 
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #mini-function(for re-scaling - puts PPG wave on 0 to 1 scale (min max... easier for viewing this way))
  range01<-function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
  
  observeEvent(input$load,{
    #browser()
    if(!is.null(input$fileIn)){
      rv$out.dir<-parseDirPath(roots=c(User=user.folder), input$dir)
      file_selected<-parseFilePaths(roots=c(wd=rv$wd, User=user.folder), input$fileIn)
      rv$file.name<-as.character(file_selected$datapath)
      if(!is.null(input$timeIn)){
        time_selected<-parseFilePaths(roots=c(wd=rv$wd, User=user.folder), input$timeIn)
        rv$time.name<-as.character(time_selected$datapath)
        #want to start by structuring the raw timing files - not zeroed
        #Will allow me to have 5 seconds of runup
        #leave the extra 5 seconds and only cut at the end (raw IBI, edited IBI & Processed PPG)
        time.file<-read.table(file=as.character(rv$time.name), sep='\t', header = T)
        if(ncol(time.file)%%2==0){
          showModal(modalDialog(
            title = 'Warning!',
            'Timing file does not contain sufficient number of columns', 
            size = 'm'
          ))
        }
        else{
          colnames(time.file)[1]<-'id'
          if(sum(time.file$id==paste(sub.id(), time.id(), sep = '_'))!=1){
            showModal(modalDialog(
              title = 'Warning!',
              'ID does not exist in timing file', 
              size = 'm'
            ))
          }
          else{
            sub.time<-time.file[time.file$id==paste(sub.id(), time.id(), sep = '_'),]
            names<-colnames(sub.time[,2:length(sub.time)])
            names<-rep(names[seq(1, length(names), by=2)], each=2)
            events<-rep(c('Start', 'End'), length(names)/2)
            evnt.labels<-paste(names, events)
            sub.temp<-as.matrix(sub.time)
            sub.temp<-as.vector(sub.temp)
            sub.temp<-as.numeric(sub.temp[2:length(sub.temp)])
            rv$sub.time<-data.frame(sub.temp, names, evnt.labels)
            colnames(rv$sub.time)<-c('Time', 'Task', 'Label')
          }
        }
        #Bring in and adjust the raw PPG file
        PPG.file<-read.table(file=as.character(file_selected$datapath), sep = '\t', header = F, skip = skip.vals())
        if(ncol(PPG.file)<col.num()){
          showModal(modalDialog(
            title = 'Warning!',
            'Number of columns incorrect. Check header and column number entry values', 
            size = 'm'
          ))
        }
        else{
          tmp<-as.numeric(PPG.file[,col.num()])
          tmp2<-resample(tmp, p=DS(), q=Hz())
          tmp2<-range01(tmp2)
          time<-0:(length(tmp2)-1)/DS()
          tmp<-data.frame(tmp2, time)
          colnames(tmp)<-c('PPG', 'Time')
          rv$PPG.proc <- tmp[tmp$Time>=min(rv$sub.time$Time) - 3 & tmp$Time<=max(rv$sub.time$Time) + 3,]
        }
      }
      else{
        PPG.file<-read.table(file=as.character(file_selected$datapath), sep = '\t', header = F, skip = skip.vals())
        if(ncol(PPG.file)<col.num()){
          showModal(modalDialog(
            title = 'Warning!',
            'Number of columns incorrect. Check header and column number entry values', 
            size = 'm'
          ))
        }
        else{
          tmp<-as.numeric(PPG.file[,col.num()])
          tmp2<-resample(tmp, p=DS(), q=Hz())
          tmp2<-range01(tmp2)
          time<-0:(length(tmp2)-1)/DS()
          tmp<-data.frame(tmp2, time)
          colnames(tmp)<-c('PPG', 'Time')
          rv$PPG.proc <- tmp 
        }
      }
    }
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Viewing Pre-processed Plot - visual verifcation
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  View.plot<-eventReactive(input$view.ppg,{
    p<-ggplot()
    if(!is.null(input$fileIn)){
      PPG.plot<-as.data.frame(rv$PPG.proc)
      MIN<-View.min()
      MAX<-View.max()
      PPG.plot<-PPG.plot[PPG.plot$Time> MIN & PPG.plot$Time <MAX,]
      p<-p+
        geom_line(aes(x=Time, y=PPG), data=PPG.plot, color='black')+
        ylab('PPG (Volts)')+
        xlab('Time (s)')
    }
    else{
      text<-'Either load data or select "View Plot"'
      p<-p+annotate("text", x = 4, y = 25, size=8, label = text)
    }
    p
  })
  
  output$PPG.check<-renderPlot({
    View.plot()
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Submitting and Processing File - Required Functions
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  
  #===========================================================================
  #Function 1 - Finding Peakings Using Specified bandwidth: 
  findpeaks <- function(vec,bw=1,x.coo=c(1:length(vec))){
    pos.x.max <- NULL
    pos.y.max <- NULL
    pos.x.min <- NULL
    pos.y.min <- NULL 	
    for(i in 1:(length(vec)-1)){ 		
      if((i+1+bw)>length(vec)){
        sup.stop <- length(vec)
      }
      else{
        sup.stop <- i+1+bw
      }
      if((i-bw)<1){
        inf.stop <- 1
      }
      else{
        inf.stop <- i-bw
      }
      subset.sup <- vec[(i+1):sup.stop]
      subset.inf <- vec[inf.stop:(i-1)]
      is.max   <- sum(subset.inf > vec[i]) == 0
      is.nomin <- sum(subset.sup > vec[i]) == 0
      no.max   <- sum(subset.inf > vec[i]) == length(subset.inf)
      no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup)
      if(is.max & is.nomin){
        pos.x.max <- c(pos.x.max,x.coo[i])
        pos.y.max <- c(pos.y.max,vec[i])
      }
      if(no.max & no.nomin){
        pos.x.min <- c(pos.x.min,x.coo[i])
        pos.y.min <- c(pos.y.min,vec[i])
      }
    }
    return(data.frame(pos.x.max,pos.y.max))
  }
  #===========================================================================
  #Function 2 - Summing IBIs from Raw PPG file: - problem is here (to start)
  time.sum<-function(x){
    Z<-rep(NA, length(x))
    for(i in 1:length(x)){
      Z[i]<-ifelse(i==1, x[i], x[i]-x[i-1])
    }
    return(Z)  
  }
  #===========================================================================
  #Function 3 - Iterative function for getting IBIs
  iter.IBI<-function(x, ds=500){
    require(psych)
    x.smooth<-as.numeric(smooth(x))
    x.smooth<-na.omit(x.smooth)
    TIME<-0:(length(x.smooth)-1)
    x.smooth<-x.smooth-predict(lm(x.smooth~TIME))
    s<-round(seq(ds/6, ds/1.5, length.out=peak.iter()), digits = 0)
    Z<-data.frame(rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)))
    withProgress(message = 'Finding Peaks', value = 0,{
      for(i in 1:length(s)){
        IBI<-findpeaks(vec=x.smooth, bw=s[i])
        IBI<-IBI[,1]
        time<-time.sum(IBI)/ds
        Z[i,1]<-s[i]
        Z[i,2]<-sd(time)
        Z[i,3]<-max(time)-min(time)
        Z[i,4]<-rmssd(time)
        Z[i,5]<-mean(acf(time, lag.max = length(time)/20, plot = F)$acf)
        Z[i,6]<-s[i]/ds
        incProgress(1/length(s), detail = paste("Pass", i, 'out of', length(s)))
      }
    })
    colnames(Z)<-c('BW', 'SD', 'Range', 'RMSSD', 'AC', 'BW(s)')
    Z<-Z[order(Z$RMSSD, decreasing = F),]
    IBI.fin<-findpeaks(vec=x.smooth, bw=Z[1,1])-1
    IBI.fin<-IBI.fin[,1]/ds
    IBI.done<-time.sum(IBI.fin)
    IBI.comp<-list(IBI.done, Z)
    names(IBI.comp)<-c('IBI.done', 'Z')
    return(IBI.comp)
  }
  #===========================================================================
  #Function 4 - Obtaining Time Values for IBI
  sum.rev<-function(x){
    Z<-rep(NA, length(x))
    for(i in 1:length(x)){
      Z[i]<-ifelse(i==1, x[i], sum(x[1:(i-1)])+x[i])
    }
    return(Z)
  }
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Submitting and Processing File - Button action
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$submit.file,{
    #PPG data import, processing and saving
    if(!is.null(input$submit.file)){
      #browser()
      PPG.cln<-rv$PPG.proc
      #-----------------------------------------------------------
      #IBI data processing and saving
      ds<-DS()
      Hz<-Hz()
      IBI.list<-iter.IBI(PPG.cln$PPG, ds=ds)
      IBI<-IBI.list$IBI.done
      IBI.time<-sum.rev(IBI)
      IBI.file<-cbind(IBI, IBI.time+min(PPG.cln$Time))
      colnames(IBI.file)<-c('IBI', 'Time')
      rv$IBI.raw<-as.data.frame(IBI.file)
      rv$PPG.proc$Time<-rv$PPG.proc$Time-min(rv$sub.time$Time)
      rv$IBI.edit<-as.data.frame(IBI.file)
      rv$IBI.edit$Time<-rv$IBI.edit$Time-min(rv$sub.time$Time)
      rv$sub.time$Time<-rv$sub.time$Time-min(rv$sub.time$Time)
      rv$tab<-head(as.data.frame(IBI.list$Z))
      rv$tab.comp<-as.data.frame(IBI.list$Z)
    } 
  })
  
  output$Iteration.Data<-renderTable({rv$tab})
  output$Event.Data<-renderTable({rv$sub.time})
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Reactive Values for on/off buttons on Editing Panel 
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$base.in, {
    #browser()
    if(!is.null(input$base.in)){
      if(rv$base.on==0 & rv$adv.on==0){
        rv$base.on<-1
      }
      else if(rv$base.on==0 & rv$adv.on==1){
        rv$base.on<-0
      }
      else if(rv$base.on==1){
        rv$base.on<-0
      }
    }
    else{
      rv$base.on<-rv$base.on
    }
  })
  
  observeEvent(input$adv.in, {
    if(!is.null(input$adv.in)){
      if(rv$adv.on==0 & rv$base.on==0){
        rv$adv.on<-1
      }
      else if(rv$adv.on==0 & rv$base.on==1){
        rv$adv.on<-0
      }
      else if(rv$adv.on==1){
        rv$adv.on<-0
        if(!is.null(rv$pred.sim)){
          rv$pred.sim<-NULL
        }
        else if(!is.null(rv$rf20)){
          rv$rf20<-NULL
        }
      }
    }
    else{
      rv$adv.on<-rv$adv.on
    }
  })
  
  observeEvent(input$select.in, {
    if(!is.null(input$select.in)){
      if(rv$select.on==0 & rv$add.delete.on==0){
        rv$select.on<-1
      }
      else if(rv$select.on==0 & rv$add.delete.on==1){
        rv$select.on<-0
      }
      else if(rv$select.on==1){
        rv$select.on<-0
      }
      else {
        rv$select.on<-rv$select.on
      }
    }
  })
  
  observeEvent(input$select.in2, {
    if(!is.null(input$select.in2)){
      if(rv$select.on2==0 & rv$add.delete.on2==0){
        rv$select.on2<-1
      }
      else if(rv$select.on2==0 & rv$add.delete.on2==1){
        rv$select.on2<-0
      }
      else if(rv$select.on2==1){
        rv$select.on2<-0
      }
      else {
        rv$select.on2<-rv$select.on2
      }
    }
  })
  
  observeEvent(input$add.delete.in, {
    if(!is.null(input$add.delete.in)){
      if(rv$add.delete.on==0 & rv$select.on==0){
        rv$add.delete.on<-1
      }
      else if(rv$add.delete.on==0 & rv$select.on==1){
        rv$add.delete.on<-0
      }
      else if(rv$add.delete.on==1){
        rv$add.delete.on<-0
      }
      else {
        rv$add.delete.on<-rv$add.delete.on
      }
    }
  })
  
  observeEvent(input$add.delete.in2, {
    if(!is.null(input$add.delete.in2)){
      if(rv$add.delete.on2==0 & rv$select.on2==0){
        rv$add.delete.on2<-1
      }
      else if(rv$add.delete.on2==0 & rv$select.on2==1){
        rv$add.delete.on2<-0
      }
      else if(rv$add.delete.on2==1){
        rv$add.delete.on2<-0
      }
      else {
        rv$add.delete.on2<-rv$add.delete.on2
      }
    }
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Color switching for buttons on Editing Panel (render UI) - base functions
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #For Base functions
  output$base.on<-renderUI({
    if(rv$base.on==0){
      tags$button(id = 'base.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'On/Off'
      )
    }
    else if(rv$base.on==1){
      tags$button(id = 'base.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'On/Off'
      )
    }
  })
  
  output$add.on<-renderUI({
    if(rv$base.on==0){
      tags$button(id = 'add.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Combine'
      )
    }
    else if(rv$base.on==1){
      tags$button(id = 'add.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'Combine'
      )
    }
  })
  
  output$divide.on<-renderUI({
    if(rv$base.on==0){
      tags$button(id = 'divide.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Divide'
      )
    }
    else if(rv$base.on==1){
      tags$button(id = 'divide.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'Divide'
      )
    }
  })
  
  output$average.on<-renderUI({
    if(rv$base.on==0){
      tags$button(id = 'average.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Average'
      )
    }
    else if(rv$base.on==1){
      tags$button(id = 'average.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'Average'
      )
    }
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Color switching for buttons on Editing Panel (render UI) - Advanced Functions
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  output$adv.on<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'adv.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'On/Off'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'adv.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'On/Off'
      )
    }
  })
  
  output$sim.on<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'sim.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Band + cosine Sim'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'sim.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'Band + cosine Sim'
      )
    }
  })
  
  output$predict.on<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'rf20.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Low Hz Stop'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'rf20.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'Low Hz Stop'
      )
    }
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Color switching for buttons on Editing Panel (render UI) - Clicking functions
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  output$select.on<-renderUI({
    if(rv$select.on==0){
      tags$button(id = 'select.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #D8D8D8; border-color: #FFFFFF",
                  'Select Off'
      )
    }
    else if(rv$select.on==1){
      tags$button(id = 'select.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #82FA58; border-color: #FFFFFF",
                  'Select On'
      )
    }
  })
  
  output$select.on2<-renderUI({
    if(rv$select.on2==0){
      tags$button(id = 'select.in2',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #D8D8D8; border-color: #FFFFFF",
                  'Select Off'
      )
    }
    else if(rv$select.on2==1){
      tags$button(id = 'select.in2',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #82FA58; border-color: #FFFFFF",
                  'Select On'
      )
    }
  })
  
  output$add.delete.on<-renderUI({
    if(rv$add.delete.on==0){
      tags$button(id = 'add.delete.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #D8D8D8; border-color: #FFFFFF",
                  'Add/Delete Off'
      )
    }
    else if(rv$add.delete.on==1){
      tags$button(id = 'add.delete.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #82FA58; border-color: #FFFFFF",
                  'Add/Delete On'
      )
    }
  })
  
  output$add.delete.on2<-renderUI({
    if(rv$add.delete.on2==0){
      tags$button(id = 'add.delete.in2',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #D8D8D8; border-color: #FFFFFF",
                  'Add/Delete Off'
      )
    }
    else if(rv$add.delete.on2==1){
      tags$button(id = 'add.delete.in2',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #82FA58; border-color: #FFFFFF",
                  'Add/Delete On'
      )
    }
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Setting up basic plotting environment
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  
  output$IBI <- output$IBI2 <- renderPlot({
    #browser()
    p.IBI<-ggplot()
    if(!is.null(rv$IBI.edit)){
      p.IBI<-ggplot(data = rv$IBI.edit, aes(x=Time, y=IBI))+
        geom_line(aes(x=Time, y=PPG), data=rv$PPG.proc, col='gray80')+
        geom_point(col="red")+
        geom_line(col="black")+
        geom_vline(aes(xintercept=Time), data=rv$IBI.edit, color = 'red', lty='dashed', alpha=.25)+
        xlab('Time(s)')+
        ylab('IBI(s)')
      if(!is.null(rv$sub.time)){
        p.IBI<-p.IBI+geom_vline(aes(xintercept=Time, color=Task), data=rv$sub.time, show.legend = F)+
          geom_text(aes(x=Time, label=Label, color=Task, y=min(rv$PPG.proc$PPG)), data = rv$sub.time, show.legend = F,
                    angle = 60, hjust=0)
      }
      if(!is.null(input$zoom_brush)){
        p.IBI<-p.IBI+coord_cartesian(xlim = c(input$zoom_brush$xmin, input$zoom_brush$xmax), 
                                     ylim = c(0, max(rv$IBI.edit$IBI)+.2))
        if(!is.null(input$select_cases)){
          temp<-brushedPoints(rv$IBI.edit, input$select_cases)
          p.IBI<-p.IBI+geom_point(aes(x=Time, y=IBI), data=temp, col='#82FA58')
        }
      }
      if(!is.null(rv$pred.sim) & rv$adv.on==1){
        p.IBI<-p.IBI+geom_line(aes(x=Time, y=PPG.adj), data=rv$pred.sim, col='#82FA58')
      }
      if(!is.null(rv$rf20) & rv$adv.on==1){
        p.IBI<-p.IBI+geom_line(aes(x=Time, y=PPG.adj), data = rv$rf20, col='#58D3F7')
      }
    }
    p.IBI
  })
  
  output$PPG_overall<-renderPlot({
    #browser()
    p.PPG2<-ggplot()
    if(!is.null(rv$IBI.edit)){
      p.PPG2<-p.PPG2+
        geom_line(aes(x=Time, y=PPG), data=rv$PPG.proc, color='black')+
        ylab('Volts')+xlab('Time(s)')  
    }
    p.PPG2
  })
  
  output$hover_info <- renderPrint({
    temp.points<-nearPoints(df=rv$IBI.edit, coordinfo = input$plot_hover, maxpoints = 1)
    mean.HR<-NA
    if(!is.null(rv$IBI.edit)){
      mean.HR<-1/mean(rv$IBI.edit$IBI[5:length(rv$IBI.edit$IBI)-5])*60
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
    temp.points<-nearPoints(df=rv$IBI.edit, coordinfo = input$plot_hover2, maxpoints = 1)
    mean.HR<-NA
    if(!is.null(rv$IBI.edit)){
      mean.HR<-1/mean(rv$IBI.edit$IBI[5:length(rv$IBI.edit$IBI)-5])*60
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
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Setting up default functions - deleting and adding cases manually
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$Peak_click, {
    #browser()
    if(!is.null(input$Peak_click) & rv$add.delete.on==1){
      Time<-c(rv$IBI.edit$Time, input$Peak_click$x)
      Time<-Time[order(Time, decreasing = F)]
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI, Time) 
      tot.edits<-cbind(rv$IBI.edit$IBI[rv$IBI.edit$Time==input$Peak_click$x],
                       input$Peak_click$x,
                       1)
      tot.edits<-as.data.frame(tot.edits)
      colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
      rv$tot.edits<-rbind(rv$tot.edits, tot.edits)
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$Peak_click2, {
    #browser()
    if(!is.null(input$Peak_click2) & rv$add.delete.on2==1){
      Time<-c(rv$IBI.edit$Time, input$Peak_click2$x)
      Time<-Time[order(Time, decreasing = F)]
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI, Time) 
      tot.edits<-cbind(rv$IBI.edit$IBI[rv$IBI.edit$Time==input$Peak_click2$x],
                       input$Peak_click2$x,
                       1)
      tot.edits<-as.data.frame(tot.edits)
      colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
      rv$tot.edits<-rbind(rv$tot.edits, tot.edits)
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$Delete, {
    #browser()
    if(!is.null(input$Delete) & rv$add.delete.on==1){
      row<-nearPoints(rv$IBI.edit, 
                      input$Delete,
                      xvar = 'Time',
                      yvar= 'IBI',
                      threshold = 5, 
                      maxpoints = 1, 
                      allRows = T)
      Time<-rv$IBI.edit$Time[row$selected_==0]
      Time<-Time[order(Time, decreasing = F)]
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI, Time)
      if(length(row$selected_==1)==0){
        rv$tot.edits<-rv$tot.edits
      }
      else if(length(row$selected_==1)>0){
        tot.edits<-c(rv$IBI.edit[row$selected_==1,],0)
        tot.edits<-as.data.frame(tot.edits)
        colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
        rv$tot.edits<-rbind(rv$tot.edits, tot.edits) 
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$Delete2, {
    #browser()
    if(!is.null(input$Delete2) & rv$add.delete.on2==1){
      row<-nearPoints(rv$IBI.edit, 
                      input$Delete2,
                      xvar = 'Time',
                      yvar= 'IBI',
                      threshold = 5, 
                      maxpoints = 1, 
                      allRows = T)
      Time<-rv$IBI.edit$Time[row$selected_==0]
      Time<-Time[order(Time, decreasing = F)]
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI, Time)
      if(length(row$selected_==1)==0){
        rv$tot.edits<-rv$tot.edits
      }
      else if(length(row$selected_==1)>0){
        tot.edits<-c(rv$IBI.edit[row$selected_==1,],0)
        tot.edits<-as.data.frame(tot.edits)
        colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
        rv$tot.edits<-rbind(rv$tot.edits, tot.edits) 
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Setting up base functions - mirroring cardio edit functions
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$add.in, {
    #browser()
    if(!is.null(input$select_cases) & rv$base.on==1){
      if(!is.null(input$add.in)){
        add<-brushedPoints(rv$IBI.edit, input$select_cases, allRows = T)
        add.temp<-rv$IBI.edit[add$selected_==1,]
        IBI<-sum(add.temp$IBI)
        Time.before<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time<add.temp$Time[1]])
        Time.after<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time>add.temp$Time[length(add.temp$Time)]])
        if(length(Time.before)==0){
          Time<-c(IBI, Time.after)
        }
        else if(length(Time.after)==0){
          Time.new<-Time.before[length(Time.before)]+IBI
          Time<-c(Time.before, Time.new)
        }
        else{
          Time.new<-Time.before[length(Time.before)]+IBI
          Time<-c(Time.before, Time.new, Time.after)
        }
        Time2<-Time-min(Time)
        IBI<-time.sum(Time2)
        rv$IBI.edit<-data.frame(IBI, Time) 
        tot.edits<-c(add.temp,2)
        tot.edits<-as.data.frame(tot.edits)
        colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
        rv$tot.edits<-rbind(rv$tot.edits, tot.edits)
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$average.in, {
    #browser()
    if(!is.null(input$select_cases) & rv$base.on==1){
      average<-brushedPoints(rv$IBI.edit, input$select_cases, allRows = T)
      average.temp<-rv$IBI.edit[average$selected_==1,]
      IBI<-mean(average.temp$IBI)*1:length(average.temp$IBI)
      Time.before<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time<min(average.temp$Time)])
      Time.after<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time>max(average.temp$Time)])
      if(length(Time.before)==0){
        Time<-c(IBI, Time.after)
      }
      else if(length(Time.after)==0){
        Time.new<-Time.before[length(Time.before)]+IBI
        Time<-c(Time.before, Time.new)
      }
      else{
        Time.new<-Time.before[length(Time.before)]+IBI
        Time<-c(Time.before, Time.new, Time.after)
      }
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI, Time) 
      tot.edits<-c(average.temp,3)
      tot.edits<-as.data.frame(tot.edits)
      colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
      rv$tot.edits<-rbind(rv$tot.edits, tot.edits)  
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$divide.in, {
    #browser()
    if(!is.null(input$select_cases) & rv$base.on==1){
      rv$denom<-round(input$divide.by, digits = 0)
      divide<-brushedPoints(rv$IBI.edit, input$select_cases, allRows = T)
      divide.temp<-rv$IBI.edit[divide$selected_==1,]
      IBI<-(divide.temp$IBI/rv$denom)*1:rv$denom
      Time.before<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time<min(divide.temp$Time)])
      Time.after<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time>max(divide.temp$Time)])
      if(length(Time.before)==0){
        Time<-c(IBI, Time.after)
      }
      else if(length(Time.after)==0){
        Time.new<-Time.before[length(Time.before)]+IBI
        Time<-c(Time.before, Time.new)
      }
      else{
        Time.new<-Time.before[length(Time.before)]+IBI
        Time<-c(Time.before, Time.new, Time.after)
      }
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI, Time) 
      tot.edits<-c(divide.temp,4)
      tot.edits<-as.data.frame(tot.edits)
      colnames(tot.edits)<-c('IBI', 'Time', 'Edit')
      rv$tot.edits<-rbind(rv$tot.edits, tot.edits)  
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Advanced Options - Predicting Values
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$sim.in,{
    #browser()
    if(!is.null(input$select_cases)& rv$adv.on==1){
      seas<-rv$PPG.proc
      min.val<-input$select_cases$xmin+3
      max.val<-input$select_cases$xmax+3
      #filter main hr frequency before messy section - high pass
      TIME.window<-round((min.val-15)*DS()):round((max.val+15)*DS())
      
      #pvals<-dnorm(TIME.window,
                   #mean=(min.val+max.val)*DS()/2, 
                   #sd=abs(min.val-max.val)*DS()/2/qnorm(.99))
      
      #pvals<-range01(pvals)
      #pvals.inv<-1-pvals
      
      low.upper<-length(seas$Time[seas$Time<=min.val-3])
      low.lower<-low.upper-99
      low.range<-low.lower:low.upper
      t<-1:DS()
      upper.lower<-length(seas$Time)-length(seas$Time[seas$Time>=max.val-3])
      upper.upper<-upper.lower+99
      upper.range<-upper.lower:upper.upper
      block.weights<-rep(0, length(seas$PPG))
      block.weights[low.range]<-t/DS()
      block.weights[upper.range]<-1-t/DS()
      block.weights[(low.upper+1):(upper.lower-1)]<-1
      block.weights.filter<-block.weights[TIME.window]
      inv.block.weights.filter<-1-block.weights.filter
      #plot(block.weights.filter)
      #Note that the +3 and -3 adjustments are necessary to connect the PPG and the IBI files via 'Time'
      sims<-2500
      DF<-data.frame()
      mean.Hz.low<-1/mean(rv$IBI.edit$IBI[rv$IBI.edit$Time>min.val-3-15 & rv$IBI.edit$Time<min.val-3])
      mean.Hz.high<-1/mean(rv$IBI.edit$IBI[rv$IBI.edit$Time>max.val-3 & rv$IBI.edit$Time<max.val-3+15])
      sd.Hz<-sd(1/rv$IBI.edit$IBI[3:(length(rv$IBI.edit$IBI)-3)], na.rm = T)
      N<-length(rv$IBI.edit$IBI[rv$IBI.edit$Time>min.val-3 & rv$IBI.edit$Time<max.val-3])
      se.Hz<-sd.Hz/sqrt(N)
      low.range.IBI<-rv$IBI.edit[rv$IBI.edit$Time>min.val-15-3 & rv$IBI.edit$Time<min.val-3,]
      cos.wv.Hz.low<-(low.range.IBI$Time[length(low.range.IBI[,1])]+3)*DS()
      TIME.window.adj<-TIME.window-cos.wv.Hz.low
      withProgress(message = 'Simulating PPG Data', value = 0,{
        for(s in 1:sims){
          Hz.temp.low<-rnorm(1, mean = mean.Hz.low, sd=se.Hz)
          filter.band.temp.low<-ffilter(seas$PPG, 
                                     from = Hz.temp.low-qnorm(.975)*se.Hz, 
                                     to = Hz.temp.low+qnorm(.975)*se.Hz, f=DS())
          filter.band.fin.low<-range01(filter.band.temp.low[TIME.window])*block.weights.filter
          #
          filter.high.temp.low<-ffilter(seas$PPG, 
                                     from = Hz.temp.low+qnorm(.975)*se.Hz,
                                     to = 3*Hz.temp.low+qnorm(.975)*se.Hz,
                                     f=DS())
          filter.high.fin.low<-range01(filter.high.temp.low[TIME.window])*inv.block.weights.filter
          #
          cos.wv.low<-cos(2*pi*TIME.window.adj*Hz.temp.low/DS())
          cos.wv.low<-range01(cos.wv.low)*block.weights.filter
          #
          temp.vec<-rep(NA, length(TIME.window))
          temp.vec[inv.block.weights.filter==1]<-range01(filter.high.fin.low[inv.block.weights.filter==1])
          temp.vec[inv.block.weights.filter<1]<-range01(((filter.band.fin.low[inv.block.weights.filter<1]+
                                                            cos.wv.low[inv.block.weights.filter<1])/2+
                                                           filter.high.fin.low[inv.block.weights.filter<1]))
          DF<-rbind(DF, temp.vec[inv.block.weights.filter<1])
          #---------------------------------------------------------------------------------------------
          Hz.temp.high<-rnorm(1, mean = mean.Hz.high, sd=se.Hz)
          filter.band.temp.high<-ffilter(seas$PPG, 
                                        from = Hz.temp.high-qnorm(.975)*se.Hz, 
                                        to = Hz.temp.high+qnorm(.975)*se.Hz, f=DS())
          filter.band.fin.high<-range01(filter.band.temp.high[TIME.window])*block.weights.filter
          #
          filter.high.temp.high<-ffilter(seas$PPG, 
                                        from = Hz.temp.high+qnorm(.975)*se.Hz,
                                        to = 3*Hz.temp.high+qnorm(.975)*se.Hz,
                                        f=DS())
          filter.high.fin.high<-range01(filter.high.temp.high[TIME.window])*inv.block.weights.filter
          #
          cos.wv.high<-cos(2*pi*TIME.window.adj*Hz.temp.high/DS())
          cos.wv.high<-range01(cos.wv.high)*block.weights.filter
          #
          temp.vec<-rep(NA, length(TIME.window))
          temp.vec[inv.block.weights.filter==1]<-range01(filter.high.fin.high[inv.block.weights.filter==1])
          temp.vec[inv.block.weights.filter<1]<-range01(((filter.band.fin.high[inv.block.weights.filter<1]+
                                                            cos.wv.high[inv.block.weights.filter<1])/2+
                                                           filter.high.fin.high[inv.block.weights.filter<1]))
          DF<-rbind(DF, temp.vec[inv.block.weights.filter<1])
          incProgress(1/sims, detail = paste(round(s/sims*100), '% Complete'))
        }
      })
      #plot(filter.band.fin)
      #plot(colMeans(DF, na.rm = T), type='l')
      #lines(seas$PPG[TIME.window], col='red')
      #lines(block.weights.filter, col='green')
      #lines(inv.block.weights.filter, col='blue')

      #PPG.ts<-ts(seas$PPG[seas$Time>min.val & seas$Time<max.val])
      PPG.adj<-range01(colMeans(DF, na.rm=T))
      pred.dat<-cbind(PPG.adj, seas$Time[(low.lower+1):upper.upper])
      colnames(pred.dat)<-c('PPG.adj', 'Time')
      pred.dat<-as.data.frame(pred.dat)
      rv$pred.sim<-pred.dat
    }
  })
  
  observeEvent(input$rf20.in, {
    browser()
    if(!is.null(input$select_cases)& rv$adv.on==1){
      seas<-rv$PPG.proc
      min.val<-input$select_cases$xmin+3
      max.val<-input$select_cases$xmax+3
      TIME.window<-round((min.val-15)*DS()):round((max.val+15)*DS())
      #--
      low.upper<-length(seas$Time[seas$Time<=min.val-3])
      low.lower<-low.upper-99
      low.range<-low.lower:low.upper
      t<-1:DS()
      upper.lower<-length(seas$Time)-length(seas$Time[seas$Time>=max.val-3])
      upper.upper<-upper.lower+99
      upper.range<-upper.lower:upper.upper
      block.weights<-rep(0, length(seas$PPG))
      block.weights[low.range]<-t/DS()
      block.weights[upper.range]<-1-t/DS()
      block.weights[(low.upper+1):(upper.lower-1)]<-1
      block.weights.filter<-block.weights[TIME.window]
      inv.block.weights.filter<-1-block.weights.filter
      #--
      sims<-input$n.sims
      DF<-data.frame()
      mu.Hz<-mean(1/rv$IBI.edit$IBI[5:(length(rv$IBI.edit$IBI)-5)], na.rm = T)
      mean.Hz.low<-as.numeric(input$freq.select[1])/60
      mean.Hz.high<-as.numeric(input$freq.select[2])/60
      sd.Hz<-sd(1/rv$IBI.edit$IBI[5:(length(rv$IBI.edit$IBI)-5)], na.rm = T)
      N<-length(rv$IBI.edit$IBI[rv$IBI.edit$Time>min.val-3 & rv$IBI.edit$Time<max.val-3])
      se.Hz<-sd.Hz/sqrt(N)
      low.range.IBI<-rv$IBI.edit[rv$IBI.edit$Time>min.val-15-3 & rv$IBI.edit$Time<min.val-3,]
      cos.wv.Hz.strt<-(low.range.IBI$Time[length(low.range.IBI[,1])]+3)*DS()
      TIME.window.adj<-TIME.window-cos.wv.Hz.strt
      #--
      withProgress(message = 'Simulating PPG Data', value = 0,{
        for(s in 1:sims){
          Hz.temp<-rnorm(1,
                         mean = (mean.Hz.high+mean.Hz.low)/2, 
                         sd = (mean.Hz.high-mean.Hz.low)/2/qnorm(.975)
                         )
          filter.stop.temp<-ffilter(seas$PPG,
                                    from = Hz.temp-qnorm(.975)*se.Hz,
                                    to = Hz.temp+qnorm(.975)*se.Hz,
                                    bandpass = F,
                                    f=DS())
          
          filter.stop.fin<-as.vector(range01(filter.stop.temp[TIME.window]))*inv.block.weights.filter
          #
          filter.band.temp<-ffilter(seas$PPG, 
                                    from = Hz.temp-qnorm(.975)*se.Hz,
                                    to = Hz.temp+qnorm(.975)*se.Hz,
                                    f=DS())
          filter.band.fin<-as.vector(range01(filter.band.temp[TIME.window]))*block.weights.filter
          #
          cos.wv<-cos(2*pi*TIME.window.adj*Hz.temp/DS())
          cos.wv<-range01(cos.wv)*block.weights.filter
          #
          temp.vec<-rep(NA, length(TIME.window))
          temp.vec[inv.block.weights.filter==1]<-range01(filter.stop.fin[inv.block.weights.filter==1])
          temp.vec[inv.block.weights.filter<1]<-range01(((filter.band.fin[inv.block.weights.filter<1]+
                                                            cos.wv[inv.block.weights.filter<1])/2+
                                                           filter.stop.fin[inv.block.weights.filter<1]))
          DF<-rbind(DF, temp.vec[inv.block.weights.filter<1])
          #---------------------------------------------------------------------------------------------
          Hz.temp<-rnorm(1,
                         mean = mu.Hz, 
                         sd = se.Hz
                         )
          filter.stop.temp<-ffilter(seas$PPG,
                                    from = Hz.temp-qnorm(.975)*se.Hz,
                                    to = Hz.temp+qnorm(.975)*se.Hz,
                                    bandpass = F,
                                    f=DS())
          
          filter.stop.fin<-as.vector(range01(filter.stop.temp[TIME.window]))*inv.block.weights.filter
          #
          filter.band.temp<-ffilter(seas$PPG, 
                                    from = Hz.temp-qnorm(.975)*se.Hz,
                                    to = Hz.temp+qnorm(.975)*se.Hz,
                                    f=DS())
          filter.band.fin<-as.vector(range01(filter.band.temp[TIME.window]))*block.weights.filter
          #
          cos.wv<-cos(2*pi*TIME.window.adj*Hz.temp/DS())
          cos.wv<-range01(cos.wv)*block.weights.filter
          #
          temp.vec<-rep(NA, length(TIME.window))
          temp.vec[inv.block.weights.filter==1]<-range01(filter.stop.fin[inv.block.weights.filter==1])
          temp.vec[inv.block.weights.filter<1]<-range01(((filter.band.fin[inv.block.weights.filter<1]+
                                                            cos.wv[inv.block.weights.filter<1])/2+
                                                           filter.stop.fin[inv.block.weights.filter<1]))
          DF<-rbind(DF, temp.vec[inv.block.weights.filter<1])
          incProgress(1/sims, detail = paste(round(s/sims*100), '% Complete'))
        }
      })
      #plot(filter.band.fin)
      #plot(colMeans(DF, na.rm = T), type='l')
      #lines(seas$PPG[TIME.window], col='red')
      #lines(block.weights.filter, col='green')
      #lines(inv.block.weights.filter, col='blue')
      
      #PPG.ts<-ts(seas$PPG[seas$Time>min.val & seas$Time<max.val])
      PPG.adj<-range01(colMeans(DF, na.rm=T))
      pred.dat<-cbind(PPG.adj, seas$Time[(low.lower+1):upper.upper])
      colnames(pred.dat)<-c('PPG.adj', 'Time')
      pred.dat<-as.data.frame(pred.dat)
      rv$rf20<-pred.dat
    }
  })

  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Saving & Closing - Case Processing Summaries
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  
  observeEvent(input$save, {
    if(!is.null(input$save)){
      #browser()
      #Prepping relevant information for summary document
      sub.dir<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output/', sep = '_'))
      dirList<-list.dirs(rv$out.dir)
      sub.dir2<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output', sep = '_'))
      if(sum(dirList==sub.dir2)==0){dir.create(sub.dir)}
      sampling<-cbind(Hz(), DS())
      colnames(sampling)<-c('Original Hz', 'Down-sampled Hz')
      #--
      edits.cnt<-unique(rv$tot.edits[,1:2])
      edits<-length(unique(edits.cnt[,1]))
      orig.IBI<-length(rv$IBI.raw[rv$IBI.raw$Time>=min(rv$sub.time$Time) & rv$IBI.raw$Time<=max(rv$sub.time$Time),1])
      fin.IBI<-length(rv$IBI.edit[rv$IBI.edit$Time>=min(rv$sub.time$Time) & rv$IBI.edit$Time<=max(rv$sub.time$Time),1])
      p.new.edits<-edits/fin.IBI
      edit.summary<-c(edits, orig.IBI, fin.IBI, round(p.new.edits,4))
      values<-c('Total Edits','Total Original IBIs','Total Edited IBIs','Proportion of Edits')
      edit.summary<-cbind(values, edit.summary)
      colnames(edit.summary)<-c('Measure', 'Value')
      #--
      rv$tot.edits$Edit[rv$tot.edits$Edit==0]<-'Deleted'
      rv$tot.edits$Edit[rv$tot.edits$Edit==1]<-'Added'
      rv$tot.edits$Edit[rv$tot.edits$Edit==2]<-'Combine'
      rv$tot.edits$Edit[rv$tot.edits$Edit==3]<-'Average'
      rv$tot.edits$Edit[rv$tot.edits$Edit==4]<-'Divide'
      edit.pnts<-cbind(round(rv$tot.edits[,1:2], digits=5), rv$tot.edits[,3])
      colnames(edit.pnts)<-c('Original IBI value', 'Time', 'Edit Type')
      #--
      RMSSD<-rmssd(rv$IBI.edit$IBI)
      SD<-sd(rv$IBI.edit$IBI)
      meanHP<-mean(rv$IBI.edit$IBI)
      meanHR<-1/mean(rv$IBI.edit$IBI)*60
      IBI.summary<-c(round(RMSSD, 5), round(SD, 5), round(meanHP,5), round(meanHR,2))
      values<-c('RMSSD', 'SD','Mean Heart Period', 'Mean BPM')
      IBI.summary<-cbind(values, IBI.summary)
      colnames(IBI.summary)<-c('Measure', 'Value')
      #------------------------------------------------------
      #stats by task 
      Task.un<-unique(rv$sub.time$Task)
      task.rmssd<-vector()
      task.hp<-vector()
      task.sd<-vector()
      tot.IBI<-vector()
      task.edits<-vector()
      #browser()
      for(i in 1:length(unique(rv$sub.time$Task))){
        tmp<-rv$sub.time[rv$sub.time$Task==Task.un[i],]
        tmp.edit<-rv$tot.edits[rv$tot.edits$Time>tmp$Time[1] & rv$tot.edits$Time<tmp$Time[2],]
        task.rmssd<-c(task.rmssd, rmssd(rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]))
        task.hp<-c(task.hp, mean(rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]))
        task.sd<-c(task.sd, sd(rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]))
        tmp.IBI<-rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]
        tot.IBI<-c(tot.IBI, length(tmp.IBI[,1]))
        task.edits<-c(task.edits, length(unique(tmp.edit[,2])))
        tmp.IBI.raw<-rv$I--BI.raw[rv$IBI.raw$Time>tmp$Time[1] & rv$IBI.raw$Time<tmp$Time[2]]
        tmp.PPG<-rv$PPG.proc[rv$PPG.proc$Time>tmp$Time[1] & rv$PPG.proc$Time<tmp$Time[2]]
        write.table(tmp.IBI, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                               Task.un[i],'IBI_edited.txt', sep = '_')))
        write.table(tmp.IBI.raw, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                  Task.un[i],'IBI_raw.txt', sep = '_')))
        write.table(tmp.PPG, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                      Task.un[i], DS(), 'Hz',
                                                                      'PPG.txt', sep = '_')))
      }
      p.edits<-task.edits/tot.IBI
      task.DF<-data.frame(rep(paste(sub.id(), time.id(), study.id(), sep='_'), length(Task.un)),
                          Task.un, 
                          round(task.rmssd, digits=5), 
                          round(task.hp, digits=5), 
                          round(task.sd, digits=5), 
                          round(tot.IBI, digits=5), 
                          task.edits, 
                          round(p.edits, digits = 5))
      colnames(task.DF)<-c('id', 'Task', 'RMSSD', 'HP', 'SD', 'Total IBIs', 'Total edits', 'Proportion Edits')
      #------------------------------------------------------
      #stats by epoch
      epoch.length<-epoch()
      for(e in 1:length(epoch.length)){
        time.vals<-seq(min(rv$sub.time$Time), max(rv$sub.time$Time), by=epoch.length[e])
        epoch.rmssd<-vector()
        epoch.hp<-vector()
        epoch.sd<-vector()
        #browser()
        for(i in 1:(length(time.vals)-1)){
          min.val<-time.vals[i]
          max.val<-time.vals[i+1]
          epoch.rmssd<-c(epoch.rmssd, rmssd(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min.val & rv$IBI.edit$Time<max.val]))
          epoch.hp<-c(epoch.hp, mean(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min.val& rv$IBI.edit$Time<max.val]))
          epoch.sd<-c(epoch.sd, sd(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min.val & rv$IBI.edit$Time<max.val]))
        }
        #browser()
        epoch.DF<-data.frame(rep(paste(sub.id(), time.id(), study.id(), sep='_'), length(time.vals)-1),
                             round(time.vals[1:(length(time.vals)-1)]-min(rv$sub.time$Time)), 
                             round(epoch.rmssd, digits = 7), 
                             round(epoch.hp, digits = 7), 
                             round(epoch.sd, digits = 7))
        colnames(epoch.DF)<-c('id', 'epoch', 'RMSSD', 'HP', 'SD')
        write.csv(epoch.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                                 paste0(epoch.length[e], 's'),
                                                                 'Epochs.csv', sep = '_')))
      }
      
      #------------------------------------------------------
      rtffile <- RTF(paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                   'Cases Processing Summary.rtf', sep = '_')))
      addParagraph(rtffile, 'IBI VizEdit v0.5\nAuthor: Matthew G. Barstead\n(c) 2017')
      addParagraph(rtffile, paste('Completion Date and Time:', Sys.time(), '\n',
                                  'Total Editing Time:', Sys.time()-rv$start.time))
      addParagraph(rtffile, paste('Edited by:', editor.id()))
      addParagraph(rtffile, paste('\n\nIBI VizEdit Summary:', sub.id(), study.id(), time.id()))
      addParagraph(rtffile, "\n\nTable 1:\nPeak Detection Processing Summary")
      addTable(rtffile, as.data.frame(round(rv$tab.comp, digits = 3)))
      addParagraph(rtffile, "\n\nTable 2:\n Samping Rate Summary")
      addTable(rtffile, sampling)
      addParagraph(rtffile, '\n\nTable 3:\nEditing Summary')
      addTable(rtffile, edit.summary)
      addParagraph(rtffile, '\n\nTable 4:\nEdited IBI File Properties')
      addTable(rtffile, IBI.summary)
      addParagraph(rtffile, '\n\nTable 5:\nEdited IBI File Properties by Task')
      addTable(rtffile, task.DF)
      addParagraph(rtffile, '\n\nTable 6:\nPoint Editing Summary')
      addTable(rtffile, edit.pnts)
      done(rtffile)
      write.table(rv$IBI.edit[rv$IBI.edit$Time>=min(rv$sub.time$Time) & rv$IBI.edit$Time<=max(rv$sub.time$Time),], 
                  paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                             'edited',
                                                             'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.csv(task.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                               'Task.csv', sep = '_')))
      write.table(rv$IBI.raw, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), 'raw',
                                                         'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.table(rv$PPG.proc, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), paste0(DS(),'Hz'), 
                                                          'PPG.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      #This is where I need to split output by Segment
      
    }
  })
  
  observeEvent(input$save.close, {
    if(!is.null(input$save.close)){
      #browser()
      #Prepping relevant information for summary document
      sub.dir<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output/', sep = '_'))
      dirList<-list.dirs(rv$out.dir)
      sub.dir2<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output', sep = '_'))
      if(sum(dirList==sub.dir2)==0){dir.create(sub.dir)}
      sampling<-cbind(Hz(), DS())
      colnames(sampling)<-c('Original Hz', 'Down-sampled Hz')
      #--
      edits.cnt<-unique(rv$tot.edits[,1:2])
      edits<-length(unique(edits.cnt[,1]))
      orig.IBI<-length(rv$IBI.raw[rv$IBI.raw$Time>=min(rv$sub.time$Time) & rv$IBI.raw$Time<=max(rv$sub.time$Time),1])
      fin.IBI<-length(rv$IBI.edit[rv$IBI.edit$Time>=min(rv$sub.time$Time) & rv$IBI.edit$Time<=max(rv$sub.time$Time),1])
      p.new.edits<-edits/fin.IBI
      edit.summary<-c(edits, orig.IBI, fin.IBI, round(p.new.edits,4))
      values<-c('Total Edits','Total Original IBIs','Total Edited IBIs','Proportion of Edits')
      edit.summary<-cbind(values, edit.summary)
      colnames(edit.summary)<-c('Measure', 'Value')
      #--
      rv$tot.edits$Edit[rv$tot.edits$Edit==0]<-'Deleted'
      rv$tot.edits$Edit[rv$tot.edits$Edit==1]<-'Added'
      rv$tot.edits$Edit[rv$tot.edits$Edit==2]<-'Combine'
      rv$tot.edits$Edit[rv$tot.edits$Edit==3]<-'Average'
      rv$tot.edits$Edit[rv$tot.edits$Edit==4]<-'Divide'
      edit.pnts<-cbind(round(rv$tot.edits[,1:2], digits=5), rv$tot.edits[,3])
      colnames(edit.pnts)<-c('Original IBI value', 'Time', 'Edit Type')
      #--
      RMSSD<-rmssd(rv$IBI.edit$IBI)
      SD<-sd(rv$IBI.edit$IBI)
      meanHP<-mean(rv$IBI.edit$IBI)
      meanHR<-1/mean(rv$IBI.edit$IBI)*60
      IBI.summary<-c(round(RMSSD, 5), round(SD, 5), round(meanHP,5), round(meanHR,2))
      values<-c('RMSSD', 'SD','Mean Heart Period', 'Mean BPM')
      IBI.summary<-cbind(values, IBI.summary)
      colnames(IBI.summary)<-c('Measure', 'Value')
      #------------------------------------------------------
      #stats by task 
      Task.un<-unique(rv$sub.time$Task)
      task.rmssd<-vector()
      task.hp<-vector()
      task.sd<-vector()
      tot.IBI<-vector()
      task.edits<-vector()
      #browser()
      for(i in 1:length(unique(rv$sub.time$Task))){
        tmp<-rv$sub.time[rv$sub.time$Task==Task.un[i],]
        tmp.edit<-rv$tot.edits[rv$tot.edits$Time>tmp$Time[1] & rv$tot.edits$Time<tmp$Time[2],]
        task.rmssd<-c(task.rmssd, rmssd(rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]))
        task.hp<-c(task.hp, mean(rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]))
        task.sd<-c(task.sd, sd(rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]))
        tmp.IBI<-rv$IBI.edit$IBI[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2]]
        tot.IBI<-c(tot.IBI, length(tmp.IBI[,1]))
        task.edits<-c(task.edits, length(unique(tmp.edit[,2])))
        tmp.IBI.raw<-rv$I--BI.raw[rv$IBI.raw$Time>tmp$Time[1] & rv$IBI.raw$Time<tmp$Time[2]]
        tmp.PPG<-rv$PPG.proc[rv$PPG.proc$Time>tmp$Time[1] & rv$PPG.proc$Time<tmp$Time[2]]
        write.table(tmp.IBI, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                  Task.un[i],'IBI_edited.txt', sep = '_')))
        write.table(tmp.IBI.raw, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                      Task.un[i],'IBI_raw.txt', sep = '_')))
        write.table(tmp.PPG, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                  Task.un[i], DS(), 'Hz',
                                                                  'PPG.txt', sep = '_')))
      }
      p.edits<-task.edits/tot.IBI
      task.DF<-data.frame(rep(paste(sub.id(), time.id(), study.id(), sep='_'), length(Task.un)),
                          Task.un, 
                          round(task.rmssd, digits=5), 
                          round(task.hp, digits=5), 
                          round(task.sd, digits=5), 
                          round(tot.IBI, digits=5), 
                          task.edits, 
                          round(p.edits, digits = 5))
      colnames(task.DF)<-c('id', 'Task', 'RMSSD', 'HP', 'SD', 'Total IBIs', 'Total edits', 'Proportion Edits')
      #------------------------------------------------------
      #stats by epoch
      epoch.length<-epoch()
      for(e in 1:length(epoch.length)){
        time.vals<-seq(min(rv$sub.time$Time), max(rv$sub.time$Time), by=epoch.length[e])
        epoch.rmssd<-vector()
        epoch.hp<-vector()
        epoch.sd<-vector()
        #browser()
        for(i in 1:(length(time.vals)-1)){
          min.val<-time.vals[i]
          max.val<-time.vals[i+1]
          epoch.rmssd<-c(epoch.rmssd, rmssd(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min.val & rv$IBI.edit$Time<max.val]))
          epoch.hp<-c(epoch.hp, mean(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min.val& rv$IBI.edit$Time<max.val]))
          epoch.sd<-c(epoch.sd, sd(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min.val & rv$IBI.edit$Time<max.val]))
        }
        #browser()
        epoch.DF<-data.frame(rep(paste(sub.id(), time.id(), study.id(), sep='_'), length(time.vals)-1),
                             round(time.vals[1:(length(time.vals)-1)]-min(rv$sub.time$Time)), 
                             round(epoch.rmssd, digits = 7), 
                             round(epoch.hp, digits = 7), 
                             round(epoch.sd, digits = 7))
        colnames(epoch.DF)<-c('id', 'epoch', 'RMSSD', 'HP', 'SD')
        write.csv(epoch.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                                 paste0(epoch.length[e], 's'),
                                                                 'Epochs.csv', sep = '_')))
      }
      
      #------------------------------------------------------
      rtffile <- RTF(paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                           'Cases Processing Summary.rtf', sep = '_')))
      addParagraph(rtffile, 'IBI VizEdit v0.5\nAuthor: Matthew G. Barstead\n(c) 2017')
      addParagraph(rtffile, paste('Completion Date and Time:', Sys.time(), '\n',
                                  'Total Editing Time:', Sys.time()-rv$start.time))
      addParagraph(rtffile, paste('Edited by:', editor.id()))
      addParagraph(rtffile, paste('\n\nIBI VizEdit Summary:', sub.id(), study.id(), time.id()))
      addParagraph(rtffile, "\n\nTable 1:\nPeak Detection Processing Summary")
      addTable(rtffile, as.data.frame(round(rv$tab.comp, digits = 3)))
      addParagraph(rtffile, "\n\nTable 2:\n Samping Rate Summary")
      addTable(rtffile, sampling)
      addParagraph(rtffile, '\n\nTable 3:\nEditing Summary')
      addTable(rtffile, edit.summary)
      addParagraph(rtffile, '\n\nTable 4:\nEdited IBI File Properties')
      addTable(rtffile, IBI.summary)
      addParagraph(rtffile, '\n\nTable 5:\nEdited IBI File Properties by Task')
      addTable(rtffile, task.DF)
      addParagraph(rtffile, '\n\nTable 6:\nPoint Editing Summary')
      addTable(rtffile, edit.pnts)
      done(rtffile)
      write.table(rv$IBI.edit[rv$IBI.edit$Time>=min(rv$sub.time$Time) & rv$IBI.edit$Time<=max(rv$sub.time$Time),], 
                  paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                        'edited',
                                        'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.csv(task.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                              'Task.csv', sep = '_')))
      write.table(rv$IBI.raw, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), 'raw',
                                                         'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.table(rv$PPG.proc, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), paste0(DS(),'Hz'), 
                                                          'PPG.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      stopApp()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
