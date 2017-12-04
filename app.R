rm(list = ls())
library(shiny)
#library(readr)
library(ggplot2)
source('assessment.R')
source('javascript.R')
source('read_encode.R')

#== 'Moderate'
ui <- fluidPage(
  
  titlePanel("ETC-ICM Biodiversity Assessment"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose input file'),
      selectInput('sepname','Column Separator:',c("Comma","Semi-colon","Tab")),
      #selectInput('sepname','Column Separator:',c("Semi-colon","Comma","Tab")),
      withTags({
        div(class="header", checked=NA,
            h4("Instructions"),
            p("Select the file containing input data for the assessment.
              The file must be in text format and column headers must be included.
              The required columns are:"),
            ul(
              li("Category"),
              li("Indicator"),
              li("Threshold"),
              li("Status"),
              li("Reference"),
              li("Bad")
            ),
            p("The following column is optional:"),
            ul(
              li("SpatialAssessmentUnit")
            ),
            p("The assesssment is made per Spatial Assessment Unit. If no Spatial Assessment Unit is specified, all indicators are combined in a single assessment."),
            p("See detailed instructions here:", HTML("<a href='data/ETC-ICM Biodiversity Assessment.pdf' target='_blank'>ETC-ICM Biodiversity Assessment.pdf</a>"))
            )
        
      })
      
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data", tableOutput("InDatatable")),
      tabPanel("Categories", 
               downloadButton('downloadCategories', 'Download Category Results'),
               uiOutput("Categories2")),  
      tabPanel("Assessment Units", 
               downloadButton('downloadAssessmentUnits', 'Download Assessment Unit Results'),
               uiOutput("AssessmentUnits"))
      
    ) # tabset panel
  )
  )  
  
) #fluid page

server <- function(input, output, session) {
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
  output$caption <- renderText(input$num)
  
  
  addResourcePath("data","./data/")
  
  sepchar<-reactive({
    sep<-","
    if(input$sepname=="Semi-colon"){sep<-";"}
    if(input$sepname=="Tab"){sep<-"\t"}
    return(sep)
  })
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
     filedata<-read_encode(infile$datapath, separator=sepchar())
    
    return(filedata)
  })
  
  InData <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,1)    #Individual indicator results
    return(out)
  })
  Indicators <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,2)
    return(out)
  })
  Categories <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,3)
    return(out)
  })
  AssessmentUnits <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,4)
    return(out)
  })
  
  
  CHASEplot<- reactive({
    QE<-QEdata()
    
    ymax=max(QE$ConSum,na.rm=TRUE)
    ymax=ceiling(ymax)
    if(ymax>5 & ymax<10){ymax=10}
    if(ymax>1 & ymax<5){ymax=5}
    
    if (is.null(QE)){return(NULL)}
    
    levels<-data_frame(factor(c("High","Good","Moderate","Poor","Bad"),
                              levels=c("High","Good","Moderate","Poor","Bad")),
                       c(0.0,0.5,1,5,10),
                       c(0.5,1,5,10,ymax))
    names(levels)[1] <- 'Status'
    names(levels)[2] <- 'ymin'
    names(levels)[3] <- 'ymax'
    
    levels2<-levels
    levels$x<-0.5
    levels2$x<-0.5+max(as.numeric(QE$Waterbody))
    
    levels<-rbind(levels, levels2)
    
    levels<-levels[levels$ymin<=ymax,]
    ymax2=max(levels$ymax,na.rm=TRUE)
    levels[levels$ymax==ymax2,]$ymax<-ymax    
    Palette1=c("#3399FF", "#66FF66", "#FFFF66","#FF9933","#FF6600" )
    
    p<-ggplot(data=QE,x=Waterbody,y=ConSum) + theme_bw() +
      geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
      geom_ribbon(data=levels,aes(x=x,ymin=ymin,ymax=ymax,fill=Status),alpha=0.5) +
      geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
      scale_fill_manual(name="Status", values=Palette1)+
      xlab('Waterbody')+ylab('Contamination Sum')
    return(p)
  })
  
  output$downloadAssessmentUnits <- downloadHandler(
    filename = function() { paste0('Results Assessment Units.csv') },
    content = function(file) {
      write.table(AssessmentUnits(), file, sep=sepchar(),row.names=F,na="", fileEncoding= "windows-1252")
    })
  output$downloadCategories <- downloadHandler(
    filename = function() { paste0('Results Categories.csv') },
    content = function(file) {
      write.table(Categories(), file, sep=sepchar(),row.names=F,na="", fileEncoding= "windows-1252")
    })
  
  
  output$InDatatable <- renderTable({return(InData())},na="")
  output$Indicators<- renderTable({return(Indicators())},na="",digits=3)
  output$Categories<- renderTable({return(Categories())},na="",digits=3)
  output$AssessmentUnits<- renderTable({return(AssessmentUnits())},na="")
  
  output$plot <- renderPlot({return(CHASEplot())})
  output$QEtable <- renderTable({return(QEspr())})
  output$Categories2 <- renderUI({
    list(
      tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("Categories")
    )})
}

shinyApp(ui=ui, server=server)
