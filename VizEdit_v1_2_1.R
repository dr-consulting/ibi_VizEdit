#===================================================================================================
# This is the Shiny web application IBI VizEdit - Matthew G. Barstead (c) 2017. 
# You can run the application by clicking the 'Run App' button above.
#
# By running this application you agree to the terms outlined at the link below:
# [insert weblink]
#
# Details about the processing steps are detailed at the link below: 
# https://github.com/matgbar/IBI_VizEdit
#
# Please cite the use of IBI VizEdit according to standard practices in your field when publishing
# Barstead, M. G. (2018). IBI VizEdit v.1.2: An RShiny Application [Computer software]. University of Maryland.
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
               rstan,
               rstanarm,
               bayesplot,
               MCMCvis, 
               astsa, 
               parallel,
               benchmarkme,
               doParallel)

###########################################################################################
###########################################################################################
# Begining of the UI 
ui <- shinyUI(
  fluidPage(theme = shinytheme('united'),
  titlePanel(
    'IBI VizEdit v1.2.1'
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
               shinyFilesButton(id='fileIn', 
                                title = 'Choose Heart Rate File:', 
                                label = 'Select HR File', 
                                multiple = F
                                ), 
               tags$br(),
               tags$p(textOutput(outputId = 'name')),
               shinyFilesButton(id='timeIn', 
                                title = 'Optional Timing File:', 
                                label = 'Select Timing File', 
                                multiple = F
               ),
               tags$br(),
               tags$p(textOutput(outputId = 'time.out'))
               ),
        column(3,
               tags$h2('File ID and Information:'
                       ),
               textInput(inputId='sub.id', 
                         label = 'Subject ID:'
                         ),
               textInput(inputId='time.id',
                         label = 'Time Point:'
                         ), 
               textInput(inputId='study.id',
                         label = '(Optional) Study ID:'
                         ),
               textInput(inputId='editor',
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
                                           selected = c('10', '15', '20', '30', '45'),
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
                verbatimTextOutput("hover_info"),
                tags$hr(),
                sliderInput(inputId = 'y.axis',
                            label = 'Y-Axis Min/Max:',
                            min=-2, 
                            max=5, 
                            value = c(0, 1.25), 
                            step = .1
                            ),
                actionButton(inputId = 'submit.zoom',
                             label = 'Set Y-axis',
                             color = '#58D3F7'),
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
                             min = 4, 
                             max = 10, 
                             value = 2
                             ),
                tags$hr(),
                uiOutput(outputId = 'add.delete.on', 
                         inline = T
                         ),
                uiOutput(outputId = 'select.on', 
                         inline = T
                         ),
                tags$hr(),
                tags$button(id = 'messy',
                            type = "button",
                            class = "btn action-button",
                            style = "color: #000000; background-color: #FA8258; border-color: #FFFFFF",
                            "Uneditable"
                            ),
                tags$button(id = 'restore.IBI',
                            type = "button",
                            class = "btn action-button",
                            style = "color: #000000; background-color: #FA8258; border-color: #FFFFFF",
                            "Restore IBI"
                            )
                ),
         column(9,
                plotOutput(outputId = "IBI",
                           height = '600px',
                           click = "Peak_click",
                           dblclick = "Delete",
                           brush = brushOpts(id="select_cases", delay = 750),
                           hover = hoverOpts(id="plot_hover", delay = 500)
                           )
                ),
         column(3),
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
               verbatimTextOutput(outputId = "hover_info2"
                                  ),
               tags$hr(),
               tags$p('Toggle Advanced Functions:'),
               uiOutput(outputId = 'adv.on', 
                        inline = T
                        ),
               tags$p('Select Function:'),
               uiOutput(outputId = 'ppg.erase', 
                        inline = T
                        ),
               uiOutput(outputId = 'ppg.restore', 
                        inline = T
                        ),
               uiOutput(outputId = 'seas.on', 
                        inline = T
                        ),
               uiOutput(outputId = 'GP.on',
                        inline = T
                        ),
               tags$hr(),
               numericInput(inputId = 'n.iter',
                            label = 'GP iterations',
                            value = 750, 
                            min = 500,
                            max = 3000
                            ),
               numericInput(inputId = 'n.wrm',
                            label = 'GP warmup',
                            value = 500, 
                            min = 250,
                            max = 1500
               ),
               tags$p('Warmup iterations must be less than total iterations'),
               numericInput(inputId = 'adapt.delta',
                            label = 'Delta Adaptation',
                            value = .9,
                            min = .70,
                            max=.99),
               tags$p('min delta = .70, max delta = .99; higher values can lead to slower run times'),
               tags$hr(),
               sliderInput(inputId = 'freq.select',
                           label = 'Select Target HP range',
                           min = .3, 
                           max = 1.5,
                           value = c(.5, .75),
                           post = 'IBI'
                           ),
               tags$hr(),
               uiOutput(outputId = 'add.delete.on2', 
                        inline = T
                        ),
               uiOutput(outputId = 'select.on2', 
                        inline = T
                        )
               #uiOutput(outputId = 'sel_2.on',
              #          inline=T
              #          )
               ),
        column(9,
               plotOutput(outputId = "IBI2",
                          height = '750px',
                          click = "Peak_click2",
                          dblclick = "Delete2",
                          brush = brushOpts(id="select_cases2", delay=750),
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
    pred.seas=NULL,
    select.on=0,
    add.delete.on=0,
    select.on2=0,
    add.delete.on2=0,
    start.time=NULL,
    GP.impute.tab=NULL, 
    IBI.temp=NULL
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
      text3<-paste("File Chosen:", "WARNING - no timing file selected!")
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
  PPG.zoom<-reactive(as.numeric(input$ppg.scale))
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
      rv$start.time<-Sys.time()
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
            sub.time<-sub.time[, colSums(is.na(sub.time))==0]
            names<-colnames(sub.time[,2:length(sub.time)])
            names<-rep(names[seq(1, length(names), by=2)], each=2)
            events<-rep(c('Start', 'End'), length(names)/2)
            evnt.labels<-paste(names, events)
            sub.temp<-as.matrix(sub.time)
            sub.temp<-as.vector(sub.temp)
            sub.temp<-as.numeric(sub.temp[2:length(sub.temp)])
            rv$sub.time<-data.frame(sub.temp, names, evnt.labels)
            colnames(rv$sub.time)<-c('Time', 'Task', 'Label')
            rv$sub.time2<-rv$sub.time
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
          PPG.1000<-resample(tmp, p=1000, q=Hz())
          time.1000<-0:(length(PPG.1000)-1)/1000
          rv$PPG.1000<-data.frame(PPG=PPG.1000, Time=time.1000)
          rv$PPG.1000<-rv$PPG.1000[rv$PPG.1000$Time>=min(rv$sub.time$Time, na.rm=T) - 3 & rv$PPG.1000$Time<=max(rv$sub.time$Time, na.rm=T) + 3,]
          tmp2<-resample(tmp, p=DS(), q=Hz())
          tmp2<-range01(tmp2)
          time<-0:(length(tmp2)-1)/DS()
          tmp<-data.frame(PPG=tmp2, Time=time)
          rv$PPG.proc <- tmp[tmp$Time>=min(rv$sub.time$Time, na.rm=T) - 3 & tmp$Time<=max(rv$sub.time$Time, na.rm=T) + 3,]
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
          PPG.1000<-resample(tmp, p=1000, q=Hz())
          time.1000<-0:(length(PPG.1000)-1)/1000
          rv$PPG.1000<-data.frame(PPG=PPG.1000, Time=time.1000)
          tmp2<-resample(tmp, p=DS(), q=Hz())
          tmp2<-range01(tmp2)
          time<-0:(length(tmp2)-1)/DS()
          rv$PPG.proc <-data.frame(PPG=tmp2, Time=time)
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
  findpeaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
    pks
  }
  #===========================================================================
  #Function 2 - Summing IBIs from Raw PPG file: 
  time.sum<-function(x){
    Z<-rep(NA, length(x))
    for(i in 1:length(x)){
      Z[i]<-ifelse(i==1, x[i], x[i]-x[i-1])
    }
    return(Z)  
  }
  #===========================================================================
  #Function 2b - Summing Time from IBIs
  IBI.sum<-function(x){
    Z<-rep(NA, length(x))
    for(i in 1:length(x)){
      Z[i]<-sum(x[1:i])
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
    s<-round(seq(round(ds/6), round(ds*4/3), length.out = peak.iter()))
    Z<-data.frame(rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)), 
                  rep(NA, length(s)))
    withProgress(message = 'Finding Peaks', value = 0,{
      for(i in 1:length(s)){
        IBI<-findpeaks(x.smooth, s[i])
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
    IBI.fin<-findpeaks(x.smooth, m=Z[1,1])-1
    IBI.fin<-IBI.fin/ds
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
        if(!is.null(rv$pred.seas)){
          rv$pred.seas<-NULL
        }
        else if(!is.null(rv$GP)){
          rv$GP<-NULL
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
  
  output$ppg.erase<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'ppg.erase.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'PPG Erase'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'ppg.erase.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'PPG Erase'
      )
    }
  })
  
  output$ppg.restore<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'ppg.restore.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'PPG Restore'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'ppg.restore.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'PPG Restore'
      )
    }
  })
  
  output$seas.on<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'seas.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Seasonal'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'seas.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Seasonal'
      )
    }
  })

  output$GP.on<-renderUI({
    if(rv$adv.on==0){
      tags$button(id = 'GP.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #E0F2F7; border-color: #FFFFFF",
                  'Bayesian GP'
      )
    }
    else if(rv$adv.on==1){
      tags$button(id = 'GP.in',
                  type = "button",
                  class = "btn action-button",
                  style="color: #000000; background-color: #58D3F7; border-color: #FFFFFF",
                  'Bayesian GP'
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

  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Setting up default functions - Identifying Messy Data & restoring IBI values 
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$messy, {
    if(!is.null(input$select_cases)){
      #browser()
      tmp.pnts<-brushedPoints(df=rv$IBI.edit, brush=input$select_cases, allRows = T)
      rv$IBI.edit$Vals<-ifelse(tmp.pnts$selected_==1, 'Uneditable', rv$IBI.edit$Vals)
    }
  })
  
  observeEvent(input$restore.IBI, {
    if(!is.null(input$select_cases)){
      #browser()
      xmin<-input$select_cases$xmin
      xmax<-input$select_cases$xmax
      rv$IBI.edit[rv$IBI.edit$Time<=xmax & rv$IBI.edit$Time>=xmin,]<-NA
      rv$IBI.edit<-na.omit(rv$IBI.edit)
      time.before<-max(rv$IBI.edit$Time[rv$IBI.edit$Time<input$select_cases$xmin])
      time.tmp<-rv$IBI.edit2$Time[rv$IBI.edit2$Time<=input$select_cases$xmax & rv$IBI.edit2$Time>=input$select_cases$xmin]
      if(length(time.tmp)<1){
        showModal(modalDialog(
          title = 'Warning', 
          'There are no Original IBI values in the range selected',
          size = 'm'
        ))
      }
      else{
        IBI.tmp<-vector()
        for(i in 1:length(time.tmp)){
          if(i==1){
            IBI.tmp[i]<-time.tmp[i]-time.before
          }
          else{
            IBI.tmp[i]<-time.tmp[i]-time.tmp[i-1]
          }
        }
        IBI<-data.frame(IBI=IBI.tmp, 
                        Time=time.tmp, 
                        Vals=rep('Orginal', length(time.tmp)), stringsAsFactors = F)
        rv$IBI.edit<-rbind(rv$IBI.edit, IBI)
        rv$IBI.edit<-rv$IBI.edit[order(rv$IBI.edit$Time, decreasing = F), ]
      }
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
      temp.points<-nearPoints(df=rv$PPG.1000, 
                              input$Peak_click,
                              xvar='Time',
                              yvar='PPG',
                              threshold = 5)
      if(length(temp.points[,1])<1){
        showModal(modalDialog(
          title = 'Warning', 
          'Unable to select points. Please try closer a PPG Peak',
          size = 'm'
        ))
      }
      else{
        new.time<-temp.points$Time[temp.points$PPG==max(temp.points$PPG)]
        Time<-c(rv$IBI.edit$Time, new.time)
        Time<-Time[order(Time, decreasing = F)]
        Time2<-Time-min(Time)
        IBI<-time.sum(Time2)
        Vals<-ifelse(Time==new.time, 'Add', rv$IBI.edit$Vals)
        rv$IBI.edit<-data.frame(IBI=IBI, Time=Time, Vals=Vals, stringsAsFactors = F)
        if(length(new.time)==0){
          showModal(modalDialog(
            title = 'Warning!',
            'No points selected - Try clicking closer to the PPG Waveform', 
            size = 'm'
          ))
        }
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$Peak_click2, {
    #browser()
    if(!is.null(input$Peak_click2) & rv$add.delete.on2==1){
      temp.points<-nearPoints(df=rv$PPG.proc2, 
                              input$Peak_click2,
                              xvar='Time',
                              yvar='PPG',
                              threshold = 5)
      if(length(temp.points[,1])<1){
        showModal(modalDialog(
          title = 'Warning', 
          'Unable to select points. Please try closer a PPG Peak',
          size = 'm'
        ))
      }
      else{
        new.time<-temp.points$Time[temp.points$PPG==max(temp.points$PPG)]
        Time<-c(rv$IBI.edit$Time, new.time)
        Time<-Time[order(Time, decreasing = F)]
        Time2<-Time-min(Time)
        IBI<-time.sum(Time2)
        Vals<-ifelse(Time==new.time, 'Add', rv$IBI.edit$Vals)
        rv$IBI.edit<-data.frame(IBI=IBI, Time=Time, Vals=Vals, stringsAsFactors = F)
        if(length(new.time)==0){
          showModal(modalDialog(
            title = 'Warning!',
            'No points selected - Try clicking closer to the PPG Waveform', 
            size = 'm'
          ))
        }
      }
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
      Vals<-rv$IBI.edit$Vals[row$selected_==0]
      Time<-Time[order(Time, decreasing = F)]
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI=IBI, Time=Time, Vals=Vals, stringsAsFactors = F)
      if(length(row$selected_[row$selected_==1])==0){
        showModal(modalDialog(
          title = 'Warning!',
          'No points selected for deletion - Try double-clicking closer to the IBI value', 
          size = 'm'
        ))
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
      Vals<-rv$IBI.edit$Vals[row$selected_==0]
      Time<-Time[order(Time, decreasing = F)]
      Time2<-Time-min(Time)
      IBI<-time.sum(Time2)
      rv$IBI.edit<-data.frame(IBI=IBI, Time=Time, Vals=Vals, stringsAsFactors = F)
      if(length(row$selected_[row$selected_==1])==0){
        showModal(modalDialog(
          title = 'Warning!',
          'No points selected for deletion - Try double-clicking closer to the IBI value', 
          size = 'm'
        ))
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Setting up base functions - mirroring Cardio Edit functions
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$add.in, {
    #browser()
    if(!is.null(input$select_cases) & rv$base.on==1){
      if(!is.null(input$add.in)){
        add<-brushedPoints(rv$IBI.edit, input$select_cases, allRows = T)
        add.temp<-rv$IBI.edit[add$selected_==1,]
        if(length(add.temp[,1])<=1){
          showModal(modalDialog(
            title = 'Warning!',
            'Error - Make sure that you have selected more than one IBI value', 
            size = 'm'
          ))
        }
        else if(length(add.temp[,1])>1){
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
          Vals.before<-rv$IBI.edit$Vals[rv$IBI.edit$Time<add.temp$Time[1]]
          Vals.mid<-'Combine'
          Vals.after<-rv$IBI.edit$Vals[rv$IBI.edit$Time>add.temp$Time[length(add.temp$Time)]]
          Vals<-c(Vals.before, Vals.mid, Vals.after)
          rv$IBI.edit<-data.frame(IBI=IBI, Time, Vals, stringsAsFactors = F) 
        }
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$average.in, {
    #browser()
    if(!is.null(input$select_cases) & rv$base.on==1){
      average<-brushedPoints(rv$IBI.edit, input$select_cases, allRows = T)
      average.temp<-rv$IBI.edit[average$selected_==1,]
      if(length(average.temp[,1])<=1){
        showModal(modalDialog(
          title = 'Warning!',
          'Error - Make sure that you have selected more than one IBI value', 
          size = 'm'
        ))
      }
      else if(length(average.temp[,1])>1){
        IBI.temp<-rep(mean(average.temp$IBI),length(average.temp$IBI))
        IBI.before<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time<min(average.temp$Time)])
        for(i in 1:length(IBI.temp)){
          IBI.temp[i]<-ifelse(i==1, max(IBI.before)+IBI.temp[i], IBI.temp[i-1]+IBI.temp[i])
        }
        IBI.after<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time>max(average.temp$Time)])
        Time.temp<-c(IBI.before, IBI.temp, IBI.after)
        IBI<-time.sum(Time.temp)
        Vals<-ifelse(average$selected_==1, 'Average', rv$IBI.edit$Vals)
        rv$IBI.edit<-data.frame(IBI=IBI, Time=Time.temp, Vals=Vals, stringsAsFactors = F) 
        rv$IBI.edit$IBI[1]<-0
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  observeEvent(input$divide.in, {
    #browser()
    if(!is.null(input$select_cases) & rv$base.on==1){
      rv$denom<-round(input$divide.by, digits = 0)
      divide<-brushedPoints(rv$IBI.edit, input$select_cases, allRows = T)
      divide.temp<-divide[divide$selected_==1,]
      if(length(divide.temp[,1])==0){
        showModal(modalDialog(
          title = 'Warning!',
          'Error - Make sure that you have ONE IBI value', 
          size = 'm'
        ))
      }
      else if(length(divide.temp[,1])>1){
        showModal(modalDialog(
          title = 'Warning!',
          'Error - Make sure that you have ONE IBI value', 
          size = 'm'
        ))
      }
      else if(length(divide.temp[,1])==1){
        IBI.temp<-rep(divide.temp$IBI/rv$denom, rv$denom)
        IBI.before<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time<min(divide.temp$Time)])
        for(i in 1:length(IBI.temp)){
          IBI.temp[i]<-ifelse(i==1, max(IBI.before)+IBI.temp[i], IBI.temp[i-1]+IBI.temp[i])
        }
        IBI.after<-as.vector(rv$IBI.edit$Time[rv$IBI.edit$Time>max(divide.temp$Time)])
        Time.temp<-c(IBI.before, IBI.temp, IBI.after)
        IBI<-time.sum(Time.temp)
        Vals.before<-rv$IBI.edit$Vals[rv$IBI.edit$Time<min(divide.temp$Time)]
        Vals.mid<-rep('Divide', length(IBI.temp))
        Vals.after<-rv$IBI.edit$Vals[rv$IBI.edit$Time>max(divide.temp$Time)]
        Vals<-c(Vals.before, Vals.mid, Vals.after)
        rv$IBI.edit<-data.frame(IBI=IBI, Time=Time.temp, Vals=Vals, stringsAsFactors = F) 
        rv$IBI.edit$IBI[1]<-0
      }
    }
    rv$IBI.edit<-rv$IBI.edit
  })
  
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Advanced Options - Predicting Values
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  observeEvent(input$ppg.erase.in,{
    #browser()
    if(!is.null(input$select_cases2) & rv$adv.on==1){
      time.min<-input$select_cases2$xmin
      time.max<-input$select_cases2$xmax
      rv$PPG.proc2$PPG[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-NA
      rv$PPG.proc2$Vals[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-'removed'
      #rv$PPG.proc2$Time[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-NA
    }
    else{
      rv$PPG.proc2<-rv$PPG.proc2
    }
  })
  
  observeEvent(input$ppg.restore.in,{
    #browser()
    if(!is.null(input$select_cases2) & rv$adv.on==1){
      time.min<-input$select_cases2$xmin
      time.max<-input$select_cases2$xmax
      rv$PPG.proc2$PPG[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-rv$PPG.100$PPG[rv$PPG.100$Time>time.min & rv$PPG.100$Time<time.max]
      rv$PPG.proc2$Vals[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-'original'
      rv$PPG.GP$PPG[rv$PPG.GP$Time>time.min & rv$PPG.GP$Time<time.max]<-NA
    }
    else{
      rv$PPG.proc2<-rv$PPG.proc2
    }
  })
  
  observeEvent(input$GP.in, {
    if(!is.null(input$select_cases2) & rv$adv.on==1){
      #browser()
      time.temp1<-Sys.time()
      options(mc.cores=parallel::detectCores())
      rstan_options(auto_write = TRUE)
      rv$GP.iter<-as.numeric(input$n.iter)
      rv$GP.wrm<-as.numeric(input$n.wrm)
      rv$delta<-as.numeric(input$adapt.delta)
      rv$HP.max<-1/as.numeric(input$freq.select[1])
      rv$HP.min<-1/as.numeric(input$freq.select[2])
      sigma_HP<-(rv$HP.max-rv$HP.min)/4
      mu_HP<-(rv$HP.max+rv$HP.min)/2
      time.min<-input$select_cases2$xmin
      time.max<-input$select_cases2$xmax
      PPG.temp<-rv$PPG.proc2[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max,]
      #Specifying "Xp" values
      TIME2<-PPG.temp$Time
      
      #selecting Xp values & N2 Values
      #tot.Xp.vals<-length(TIME2)
      #sel.Xp.vals<-round(seq(1, tot.Xp.vals, length.out = 25))
      #sel.Xp.vals<-unique(sel.Xp.vals)
      Xp<-TIME2
      N2<-length(Xp)
      
      #Selecting Y and N vals
      min.TIME2<-min(TIME2)
      max.TIME2<-max(TIME2)
      time.span<-max.TIME2-min.TIME2
      Y.vals<-rbind(rv$PPG.proc2[rv$PPG.proc2$Time>min.TIME2-1.5*time.span & rv$PPG.proc2$Time<min.TIME2,],
                    rv$PPG.proc2[rv$PPG.proc2$Time>max.TIME2 & rv$PPG.proc2$Time<min.TIME2+1.5*time.span,])
      Y.vals<-na.omit(Y.vals)
      tot.Y.vals<-length(Y.vals[,1])
      sel.Y.vals<-round(seq(1, tot.Y.vals, length.out = round(length(Y.vals)/DS()*4)))
      sel.Y.vals<-unique(sel.Y.vals)
      Y<-Y.vals$PPG[sel.Y.vals]
      X<-Y.vals$Time[sel.Y.vals]
      N1<-length(X)
      
      #estimating respiration - using spectral density to obtain average
      spec<-mvspec(rv$PPG.proc$PPG, 
                   spans = c(7,7), 
                   taper=.1, 
                   demean = T, 
                   log='no', 
                   plot = F)
      min.R<-12/60/DS()
      max.R<-20/60/DS()

      spec.trunc<-data.frame(freq=spec$freq[spec$freq>=min.R&spec$freq<=max.R],
                             spec=spec$spec[spec$freq>=min.R&spec$freq<=max.R])
      spec.trunc$prob<-spec.trunc$spec/sum(spec.trunc$spec)
      tmp.dist<-sample(spec.trunc$freq, size = 10000, replace = T, prob = spec.trunc$prob)*DS()
      mu_R<-mean(tmp.dist)
      sigma_R<-sd(tmp.dist)

      #Data for stan model
      dat<-list(N1=N1,
                N2=N2,
                Xp=Xp,
                X=X,
                Y=Y,
                mu_HR=mu_HP,
                mu_R=mu_R,
                sigma_HR=sigma_HP, 
                sigma_R=sigma_R
                )
      
      pars.to.monitor<-c('HR','R', 'Ypred', paste0('a',1:4), paste0('r',1:7))
      
      fit.stan<-stan(file='~/IBI_VizEdit_stan/GP_main2.stan',
                     data = dat, 
                     warmup = rv$GP.wrm,
                     iter = rv$GP.iter,
                     refresh=5,
                     chains = 3,
                     init = list(list(mu_HR=mu_HP,
                                      mu_R=mu_R),
                                 list(mu_HR=mu_HP,
                                      mu_R=mu_R),
                                 list(mu_HR=mu_HP,
                                      mu_R=mu_R)
                                 ),
                     pars = pars.to.monitor,
                     control = list(adapt_delta = rv$delta, 
                                    max_treedepth = 12)
                     )
      
      traceplot(fit.stan, pars='HR')
      mcmc_areas(as.matrix(fit.stan), pars='HR')
      mcmc_areas(as.matrix(fit.stan), pars='a4')
      
      #browser()
      #------------------------------------------------------------
      #Taking most likely posterior value from each distribution
      estimate_mode <- function(x) {
        d <- density(x)
        abs(d$x[which.max(d$y)])
      }
      #extracting values from the model: 
      HR.est<-extract(fit.stan, 'HR')
      mu_HR2<-estimate_mode(HR.est$HR)
      R.est<-extract(fit.stan, 'R')
      mu_R2<-estimate_mode(R.est$R)

      y_pred<-extract(fit.stan, 'Ypred')
      PPG.new<-colMeans(y_pred$Ypred)
      rv$PPG.proc2$PPG[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-PPG.new
      rv$PPG.proc2$Vals[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-'GP impute'
      run_time<-Sys.time()-time.temp1
      units(run_time)<-'mins'
      GP.impute<-data.frame(Time1=min(TIME2),
                            Time2=max(TIME2), 
                            Time_tot=round(max(TIME2)-min(TIME2), digits=2),
                            MAP_HR_impute=round(estimate_mode(HR.est$HR)*60, digits = 2),
                            run_time=round(run_time))
      colnames(GP.impute)<-c('Imputation Start',
                             'Imputation End',
                             'Total Time Imputed (s)',
                             'MAP Imputed HR (BPM)',
                             'Total Run Time')
      rv$GP.impute.tab<-rbind(rv$GP.impute.tab, GP.impute)
      rv$PPG.GP$PPG[rv$PPG.proc2$Time>time.min & rv$PPG.proc2$Time<time.max]<-PPG.new
      #--
      #Saving GP summary information in a separate directory
      sub.dir<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output/', sep = '_'))
      dirList<-list.dirs(rv$out.dir)
      sub.dir2<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output', sep = '_'))
      if(sum(dirList==sub.dir2)==0){dir.create(sub.dir)}
      
      sub.dir.GP<-paste0(sub.dir,'GP_summaries/')
      dirList.GP<-list.dirs(sub.dir)
      sub.dir.GP2<-paste0(sub.dir,'GP_summaries')
      if(sum(dirList==sub.dir.GP2)==0){dir.create(sub.dir.GP)}
      
      g1<-traceplot(fit.stan, pars='HR')+
        ggtitle(paste(sub.id(), time.id(), study.id(), "HR Traceplot GP Imputation:", 
                      round(time.min, digits = 2), 'to', round(time.max, digits = 2)))
      ggsave(filename = paste0(sub.dir.GP, sub.id(), time.id(), study.id(),round(time.min*100), '_', round(time.max*100), '.jpeg'),
             plot = g1, device = 'jpeg', dpi=300, width=8, height=8, units = 'in')
      
      fit.summary<-summary(fit.stan, pars=pars.to.monitor[-3], probs=c(.01, .99))$summary
      
      #browser()
      sink(paste0(sub.dir.GP, sub.id(), time.id(), study.id(), round(time.min*100), '_', round(time.max*100),'.txt'))
      cat(paste0('ID:', '\t\t\t\t', paste(sub.id(), time.id(), study.id(), sep = '_')))
      cat(paste0('\nT1:', '\t\t\t\t', round(time.min, digits = 2)))
      cat(paste0('\nT2:', '\t\t\t\t', round(time.max, digits = 2)))
      cat(paste0('\nTotal Time', '\t\t\t', round(time.max-time.min, digits = 2), '(s)'))
      cat(paste0('\nRun Time:', '\t\t\t', round(run_time, digits = 2), '(mins)'))
      cat(paste0('\nMAP HR:', '\t\t\t\t', round(mu_HR2*60, digits = 2)))
      cat(paste0('\nMAP R:', '\t\t\t\t',round(mu_R2*60, digits = 2)))
      cat(paste0('\nadapt_delta:', '\t\t\t', rv$delta))
      cat(paste0('\nIterations:', '\t\t\t', rv$GP.iter))
      cat(paste0('\nWarmup:', '\t\t\t\t', rv$GP.wrm))
      cat('\n\nSystem Information:')
      cat('\n-------------------------------------------------------------------------------------')
      cat(paste0('\nProcessor:', '\t\t\t', get_cpu()$model_name))
      cat(paste0('\nNumber of Cores:', '\t\t', detectCores(logical=F)))
      cat(paste0('\nNumber of Threads:', '\t\t', detectCores(logical=T)))
      cat(paste0('\nRAM:', '\t\t\t\t', paste(round(get_ram()/1073741824), 'GB')))
      cat('\n-------------------------------------------------------------------------------------')
      cat('\n\nGP SUMMARY:\n\n')
      print(fit.summary)
      sink()
    }
  })

  #=====================================================================================
  #-------------------------------------------------------------------------------------
  #Saving & Closing - Case Processing Summaries
  #=====================================================================================
  #-------------------------------------------------------------------------------------
  
  observeEvent(input$save, {
    if(!is.null(input$save)){
      browser()
      #Prepping relevant information for summary document
      sub.dir<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output/', sep = '_'))
      dirList<-list.dirs(rv$out.dir)
      sub.dir2<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output', sep = '_'))
      if(sum(dirList==sub.dir2)==0){dir.create(sub.dir)}
      sampling<-cbind(Hz(), DS())
      colnames(sampling)<-c('Original Hz', 'Down-sampled Hz')
      #--
      edits.cnt<-length(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable' & rv$IBI.edit$Vals!='Original'])
      orig.IBI<-length(rv$IBI.edit2$IBI[rv$IBI.edit2$Time>=min(rv$sub.time$Time, na.rm=T) & rv$IBI.edit2$Time<=max(rv$sub.time$Time, na.rm=T)])
      fin.IBI<-length(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min(rv$sub.time$Time, na.rm=T) & rv$IBI.edit$Time<=max(rv$sub.time$Time, na.rm=T)])
      p.new.edits<-edits.cnt/fin.IBI
      edit.summary<-c(edits.cnt, orig.IBI, fin.IBI, round(p.new.edits,4))
      values<-c('Total Unique Edits','Total Original IBIs','Total Final IBIs','Proportion of Edits')
      edit.summary<-cbind(values, edit.summary)
      colnames(edit.summary)<-c('Measure', 'Value')
      #--
      add.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Add',1])
      divide.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Divide',1])
      combine.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Combine',1])
      average.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Average',1])
      edit.type.names<-c("Added", 'Divided', 'Combined', 'Averaged', 'Total')
      edit.type.sum<-c(add.sum, divide.sum, combine.sum, average.sum, sum(c(add.sum, divide.sum, combine.sum, average.sum), na.rm = T))
      edit.type.p<-edit.type.sum/edit.type.sum[length(edit.type.sum)]
      edit.type.p.overall<-edit.type.sum/fin.IBI
      edit.type<-cbind(edit.type.names, edit.type.sum, round(edit.type.p*100, digits = 2), round(edit.type.p.overall*100, digits=2))
      colnames(edit.type)<-c('Edit Type', 'Total Number of Edits', 'Total Edits %', 'Overall %')
      #--
      edit.pnts<-rv$IBI.edit[rv$IBI.edit$Vals!='Uneditable' & rv$IBI.edit$Vals!='Original',]
      colnames(edit.pnts)<-c('Edited IBI Value', 'Time', 'Edit Type')
      edit.pnts[,1:2]<-round(edit.pnts[,1:2], digits = 4)
      #--
      RMSSD<-rmssd(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])
      SD<-sd(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])
      meanHP<-mean(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])
      meanHR<-1/mean(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])*60
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
      tot.editable.IBI<-vector()
      task.edits<-vector()
      #browser()
      for(i in 1:length(unique(rv$sub.time$Task))){
        tmp<-rv$sub.time[rv$sub.time$Task==Task.un[i],]
        tmp.IBI<-rv$IBI.edit[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2],]
        tmp.IBI.raw<-rv$IBI.edit2[rv$IBI.edit2$Time>tmp$Time[1] & rv$IBI.edit2$Time<tmp$Time[2],]
        tmp.PPG<-rv$PPG.proc[rv$PPG.proc$Time>tmp$Time[1] & rv$PPG.proc$Time<tmp$Time[2],]
        
        task.rmssd<-c(task.rmssd, rmssd(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable'], na.rm = T))
        task.hp<-c(task.hp, mean(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable'], na.rm = T))
        task.sd<-c(task.sd, sd(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable'], na.rm = T))
        tot.IBI<-c(tot.IBI, length(tmp.IBI[,1]))
        tot.editable.IBI<-c(tot.editable.IBI, length(tmp.IBI[tmp.IBI$Vals!='Uneditable',1]))
        task.edits<-c(task.edits, length(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable' & tmp.IBI$Vals!='Original']))
        write.table(tmp.IBI, row.names = F, sep='\t', paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                               Task.un[i],'IBI_edited.txt', sep = '_')), 
                    quote = F)
        write.table(tmp.IBI.raw, row.names = F, sep='\t', paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                  Task.un[i],'IBI_raw.txt', sep = '_')), 
                    quote=F)
        write.table(tmp.PPG, row.names = F, sep='\t', paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                      Task.un[i], DS(), 'Hz',
                                                                      'PPG.txt', sep = '_')), 
                    quote=F)
      }
      p.edits<-task.edits/tot.IBI
      p.editable<-tot.editable.IBI/tot.IBI
      task.DF<-data.frame(rep(paste(sub.id(), time.id(), study.id(), sep='_'), length(Task.un)),
                          Task.un, 
                          round(task.rmssd, digits=4), 
                          round(task.hp, digits=4), 
                          round(task.sd, digits=4), 
                          round(tot.IBI, digits=4), 
                          task.edits, 
                          round(p.edits, digits = 4), 
                          round(p.editable, digits=4))
      colnames(task.DF)<-c('id', 'Task', 'RMSSD', 'HP', 'SD', 'Total IBIs', 'Total edits', 'Proportion Edits', 'Proportion Editable')
      #removing values if there are too many uneditable sections in the data... (may be tricky at first to figure out what the right proportion is here)
      for(j in 2:6){
        for(i in 1:length(task.DF[,1]))
        if(task.DF[i,9]<=2/3){
          task.DF[i,j]<-NA
        }
      }
      
      #------------------------------------------------------
      #stats by epoch
      epoch.length<-epoch()
      for(e in 1:length(epoch.length)){
        time.vals<-seq(min(rv$sub.time$Time, na.rm=T), max(rv$sub.time$Time, na.rm=T), by=epoch.length[e])
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
                             round(time.vals[1:(length(time.vals)-1)]-min(rv$sub.time$Time, na.rm=T)), 
                             round(epoch.rmssd, digits = 7), 
                             round(epoch.hp, digits = 7), 
                             round(epoch.sd, digits = 7))
        colnames(epoch.DF)<-c('id', 'epoch', 'RMSSD', 'HP', 'SD')
        write.csv(epoch.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                                 paste0(epoch.length[e], 's'),
                                                                 'Epochs.csv', sep = '_')))
      }
      #------------------------------------------------------
      #Imputation stats for PPG file
      Impute.tab<-rv$GP.impute.tab
      tot.time<-max(rv$PPG.proc$Time)-min(rv$PPG.proc$Time)
      if(length(Impute.tab[,1])>0){
        tot.impute.time<-sum(Impute.tab$`Total Time Imputed (s)`)
      }
      else {
        tot.impute.time<-0
      }
      per.impute.time<-round(tot.impute.time/tot.time, digits=3)
      
      #------------------------------------------------------
      time.end<-Sys.time()
      edit.time<-time.end-rv$start.time
      units(edit.time)<-'mins'
      rtffile <- RTF(paste0(sub.dir, paste(sub.id(), study.id(), time.id(),
                                                   'Cases Processing Summary.rtf', sep = '_')))
      addParagraph(rtffile, 'IBI VizEdit v1.2.1\nAuthor: Matthew G. Barstead\n(c) 2018')
      addParagraph(rtffile, '--------------------------------------------------------------')
      addParagraph(rtffile, paste('Completion Date and Time:', Sys.time(),
                                  '\nTotal Editing Time:', round(edit.time, digits = 2), 
                                  'mins'))
      addParagraph(rtffile, paste('Edited by:', editor.id()))
      addParagraph(rtffile, paste('\n\nIBI VizEdit Summary:', sub.id(), study.id(), time.id()))
      addParagraph(rtffile, "\n\nTable 1:\nPeak Detection Processing Summary")
      addTable(rtffile, as.data.frame(round(rv$tab.comp, digits = 3)))
      addParagraph(rtffile, "\n\nTable 2:\n Samping Rate Summary")
      addTable(rtffile, sampling)
      addParagraph(rtffile, '\n\nTable 3:\nEditing Summary')
      addTable(rtffile, edit.summary)
      addParagraph(rtffile, '\n\nTable 4:\nEditing Summary by Edit Type')
      addTable(rtffile, edit.type)
      addParagraph(rtffile, '\n\nTable 5:\nEdited IBI File Properties')
      addTable(rtffile, IBI.summary)
      addParagraph(rtffile, '\n\nTable 6:\nEdited IBI File Properties by Task')
      addTable(rtffile, task.DF)
      addParagraph(rtffile, '\n\nTable 7:\nPoint Editing Summary')
      addTable(rtffile, edit.pnts)
      addParagraph(rtffile,'\n\nTable 8:\nGaussian Process Imputation Summary')
      if(length(Impute.tab)>0){
        addTable(rtffile, Impute.tab)
      }
      else{
        addParagraph(rtffile, 'No Gaussian process imputation used')
      }
      addParagraph(rtffile, paste('\nPercent of PPG file Imputed via GP:', 
                                  paste0(per.impute.time, '%')))
      done(rtffile)
      write.table(rv$IBI.edit[rv$IBI.edit$Time>=min(rv$sub.time$Time, na.rm=T) & rv$IBI.edit$Time<=max(rv$sub.time$Time, na.rm=T),],
                  paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                             'edited',
                                                             'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.csv(task.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                               'Task.csv', sep = '_')))
      write.table(rv$IBI.edit2, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), 'raw',
                                                         'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.table(rv$PPG.proc, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), paste0(DS(),'Hz'), 
                                                          'PPG.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
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
      edits.cnt<-length(rv$IBI.edit[rv$IBI.edit$Vals!='Uneditable' & rv$IBI.edit$Vals!='Original',1])
      orig.IBI<-length(rv$IBI.edit2$IBI[rv$IBI.edit2$Time>=min(rv$sub.time$Time, na.rm=T) & rv$IBI.edit2$Time<=max(rv$sub.time$Time, na.rm=T)])
      fin.IBI<-length(rv$IBI.edit$IBI[rv$IBI.edit$Time>=min(rv$sub.time$Time, na.rm=T) & rv$IBI.edit$Time<=max(rv$sub.time$Time, na.rm=T)])
      p.new.edits<-edits.cnt/fin.IBI
      edit.summary<-c(edits.cnt, orig.IBI, fin.IBI, round(p.new.edits,4))
      values<-c('Total Unique Edits','Total Original IBIs','Total Final IBIs','Proportion of Edits')
      edit.summary<-cbind(values, edit.summary)
      colnames(edit.summary)<-c('Measure', 'Value')
      #--
      add.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Add',1])
      divide.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Divide',1])
      combine.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Combine',1])
      average.sum<-length(rv$IBI.edit[rv$IBI.edit$Vals=='Average',1])
      edit.type.names<-c("Added", 'Divided', 'Combined', 'Averaged', 'Total')
      edit.type.sum<-c(add.sum, divide.sum, combine.sum, average.sum, sum(c(add.sum, divide.sum, combine.sum, average.sum), na.rm = T))
      edit.type.p<-edit.type.sum/edit.type.sum[length(edit.type.sum)]
      edit.type.p.overall<-edit.type.sum/fin.IBI
      edit.type<-cbind(edit.type.names, edit.type.sum, round(edit.type.p*100, digits = 2), round(edit.type.p.overall*100, digits=2))
      colnames(edit.type)<-c('Edit Type', 'Total Number of Edits', 'Total Edits %', 'Overall %')
      #--
      edit.pnts<-rv$IBI.edit[rv$IBI.edit$Vals!='Uneditable' & rv$IBI.edit$Vals!='Original',]
      colnames(edit.pnts)<-c('Edited IBI Value', 'Time', 'Edit Type')
      edit.pnts[,1:2]<-round(edit.pnts[,1:2], digits = 4)
      #--
      RMSSD<-rmssd(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])
      SD<-sd(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])
      meanHP<-mean(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])
      meanHR<-1/mean(rv$IBI.edit$IBI[rv$IBI.edit$Vals!='Uneditable'])*60
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
      tot.editable.IBI<-vector()
      task.edits<-vector()
      #browser()
      for(i in 1:length(unique(rv$sub.time$Task))){
        tmp<-rv$sub.time[rv$sub.time$Task==Task.un[i],]
        tmp.IBI<-rv$IBI.edit[rv$IBI.edit$Time>tmp$Time[1] & rv$IBI.edit$Time<tmp$Time[2],]
        tmp.IBI.raw<-rv$IBI.edit2[rv$IBI.edit2$Time>tmp$Time[1] & rv$IBI.edit2$Time<tmp$Time[2],]
        tmp.PPG<-rv$PPG.proc[rv$PPG.proc$Time>tmp$Time[1] & rv$PPG.proc$Time<tmp$Time[2],]
        
        task.rmssd<-c(task.rmssd, rmssd(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable'], na.rm = T))
        task.hp<-c(task.hp, mean(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable'], na.rm = T))
        task.sd<-c(task.sd, sd(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable'], na.rm = T))
        tot.IBI<-c(tot.IBI, length(tmp.IBI[,1]))
        tot.editable.IBI<-c(tot.editable.IBI, length(tmp.IBI[tmp.IBI$Vals!='Uneditable',1]))
        task.edits<-c(task.edits, length(tmp.IBI$IBI[tmp.IBI$Vals!='Uneditable' & tmp.IBI$Vals!='Original']))
        write.table(tmp.IBI, row.names = F, sep='\t', paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                            Task.un[i],'IBI_edited.txt', sep = '_')), 
                    quote = F)
        write.table(tmp.IBI.raw, row.names = F, sep='\t', paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                                Task.un[i],'IBI_raw.txt', sep = '_')), 
                    quote=F)
        write.table(tmp.PPG, row.names = F, sep='\t', paste0(sub.dir, paste(sub.id(), time.id(), study.id(),
                                                                            Task.un[i], DS(), 'Hz',
                                                                            'PPG.txt', sep = '_')), 
                    quote=F)
      }
      p.edits<-task.edits/tot.IBI
      p.editable<-tot.editable.IBI/tot.IBI
      task.DF<-data.frame(rep(paste(sub.id(), time.id(), study.id(), sep='_'), length(Task.un)),
                          Task.un, 
                          round(task.rmssd, digits=4), 
                          round(task.hp, digits=4), 
                          round(task.sd, digits=4), 
                          round(tot.IBI, digits=4), 
                          task.edits, 
                          round(p.edits, digits = 4), 
                          round(p.editable, digits=4))
      colnames(task.DF)<-c('id', 'Task', 'RMSSD', 'HP', 'SD', 'Total IBIs', 'Total edits', 'Proportion Edits', 'Proportion Editable')
      #removing values if there are too many uneditable sections in the data... (may be tricky at first to figure out what the right proportion is here)
      for(j in 2:6){
        for(i in 1:length(task.DF[,1]))
          if(task.DF[i,9]<=2/3){
            task.DF[i,j]<-NA
          }
      }
      
      #------------------------------------------------------
      #stats by epoch
      epoch.length<-epoch()
      for(e in 1:length(epoch.length)){
        time.vals<-seq(min(rv$sub.time$Time, na.rm=T), max(rv$sub.time$Time, na.rm=T), by=epoch.length[e])
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
                             round(time.vals[1:(length(time.vals)-1)]-min(rv$sub.time$Time, na.rm=T)), 
                             round(epoch.rmssd, digits = 7), 
                             round(epoch.hp, digits = 7), 
                             round(epoch.sd, digits = 7))
        colnames(epoch.DF)<-c('id', 'epoch', 'RMSSD', 'HP', 'SD')
        write.csv(epoch.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                                 paste0(epoch.length[e], 's'),
                                                                 'Epochs.csv', sep = '_')))
      }
      #------------------------------------------------------
      #Imputation stats for PPG file
      Impute.tab<-rv$GP.impute.tab
      tot.time<-max(rv$PPG.proc$Time)-min(rv$PPG.proc$Time)
      if(length(Impute.tab[,1])>0){
        tot.impute.time<-sum(Impute.tab$`Total Time Imputed (s)`)
      }
      else {
        tot.impute.time<-0
      }
      per.impute.time<-round(tot.impute.time/tot.time, digits=3)
      
      #------------------------------------------------------
      time.end<-Sys.time()
      edit.time<-time.end-rv$start.time
      units(edit.time)<-'mins'
      rtffile <- RTF(paste0(sub.dir, paste(sub.id(), study.id(), time.id(),
                                           'Cases Processing Summary.rtf', sep = '_')))
      addParagraph(rtffile, 'IBI VizEdit v1.2.1\nAuthor: Matthew G. Barstead\n(c) 2018')
      addParagraph(rtffile, '--------------------------------------------------------------')
      addParagraph(rtffile, paste('Completion Date and Time:', Sys.time(),
                                  '\nTotal Editing Time:', round(edit.time, digits = 2), 
                                  'mins'))
      addParagraph(rtffile, paste('Edited by:', editor.id()))
      addParagraph(rtffile, paste('\n\nIBI VizEdit Summary:', sub.id(), study.id(), time.id()))
      addParagraph(rtffile, "\n\nTable 1:\nPeak Detection Processing Summary")
      addTable(rtffile, as.data.frame(round(rv$tab.comp, digits = 3)))
      addParagraph(rtffile, "\n\nTable 2:\n Samping Rate Summary")
      addTable(rtffile, sampling)
      addParagraph(rtffile, '\n\nTable 3:\nEditing Summary')
      addTable(rtffile, edit.summary)
      addParagraph(rtffile, '\n\nTable 4:\nEditing Summary by Edit Type')
      addTable(rtffile, edit.type)
      addParagraph(rtffile, '\n\nTable 5:\nEdited IBI File Properties')
      addTable(rtffile, IBI.summary)
      addParagraph(rtffile, '\n\nTable 6:\nEdited IBI File Properties by Task')
      addTable(rtffile, task.DF)
      addParagraph(rtffile, '\n\nTable 7:\nPoint Editing Summary')
      addTable(rtffile, edit.pnts)
      addParagraph(rtffile,'\n\nTable 8:\nGaussian Process Imputation Summary')
      if(length(Impute.tab)>0){
        addTable(rtffile, Impute.tab)
      }
      else{
        addParagraph(rtffile, 'No Gaussian process imputation used')
      }
      addParagraph(rtffile, paste('\nPercent of PPG file Imputed via GP:', 
                                  paste0(per.impute.time, '%')))
      done(rtffile)
      write.table(rv$IBI.edit[rv$IBI.edit$Time>=min(rv$sub.time$Time, na.rm=T) & rv$IBI.edit$Time<=max(rv$sub.time$Time, na.rm=T),],
                  paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                        'edited',
                                        'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.csv(task.DF, row.names = F, paste0(sub.dir, paste(sub.id(), time.id(), study.id(), 
                                                              'Task.csv', sep = '_')))
      write.table(rv$IBI.edit2, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), 'raw',
                                                           'IBI.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      write.table(rv$PPG.proc, paste0(sub.dir, '/', paste(sub.id(), study.id(), time.id(), paste0(DS(),'Hz'), 
                                                          'PPG.txt', sep = '_')), 
                  row.names = F, quote = F, sep='\t')
      #==============================================
      stopApp()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
