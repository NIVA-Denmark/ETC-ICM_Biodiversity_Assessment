rm(list = ls())
library(shiny)
library(readr)
library(ggplot2)
source('assessment.R')
source('javascript.R')


#== 'Moderate'
ui <- fluidPage(
  
  titlePanel("ETC-ICM Biodiversity Assessment"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose input file'),
      #selectInput('sepname','Column Separator:',c("Comma","Semi-colon","Tab")),
      selectInput('sepname','Column Separator:',c("Semi-colon","Comma","Tab")),
      withTags({
        div(class="header", checked=NA,
            h4("Instructions"),
            p("Select the file containing input data for the assessment.
              The file must be in text format and column headers must be included.
              The required columns are:"),
            ul(
              li("Category"),
              li("Indicator"),
              li("Bad"),
              li("Threshold"),
              li("Reference"),
              li("Status")
            ),
            p("The following column is optional:"),
            ul(
              li("SpatialAssessmentUnit")
            ),
            p("The assesssment is made per Spatial Assessment Unit. If no Spatial Assessment Unit is specified, all indicators are combined in a single assessment."),
            p("See detailed instructions here:", HTML("<a href='data/ETC-ICM Biodiversity Assessment.pdf' target='_blank'>ETC-ICM Biodiversity Assessment.pdf</a>"))
        )
        
      })#,
      
      # withTags({
      #   div(class="header", checked=NA,
      #       h4("More information"),
      #       p("To find out more, contact ",
      #         a(href="https://niva-denmark.dk/", "NIVA Denmark", target="_blank")),
      #       a(href="https://niva-denmark.dk/",img(src="NIVA-Denmark-150.png", target="_blank"))
      #   )
      # })
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("InDatatable")),
        tabPanel("Categories",
                 #downloadButton('downloadCategories', 'Download'),
                 uiOutput("tab2"),
                 uiOutput("Categories2")),
        tabPanel("Assessment Units",
                 #downloadButton('downloadAssessmentUnits', 'Download'),
                 uiOutput("tab3"),
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

    
    dfencode<-guess_encoding(infile$datapath,n_max=-1)
    cat(paste0(dfencode$encoding[1],"\n"))
    filedata<-read.table(infile$datapath, sep=sepchar(),
                         encoding=dfencode$encoding[1], header=T, stringsAsFactors=F)
    
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
  
  
  output$InDatatable <- renderTable({return(InData())},na="")
  output$Categories<- renderTable({return(Categories())},na="",digits=3)
  output$AssessmentUnits<- renderTable({return(AssessmentUnits())},na="",digits=3)
  
  output$downloadAssessmentUnits <- downloadHandler(
    filename = function() { paste0('Results Assessment Units.csv') },
    content = function(file) {
      write.table(AssessmentUnits(), file, sep=sepchar(),row.names=F,na="")
    })
  output$downloadCategories <- downloadHandler(
    filename = function() { paste0('Results Categories.csv') },
    content = function(file) {
      write.table(Categories(), file, sep=sepchar(),row.names=F,na="")
    })
  
  output$tab2 <- renderUI({
    if(is.null(filedata())){NULL}
    else{downloadButton('downloadCategories', 'Download')}
  })
  output$tab3 <- renderUI({
    if(is.null(filedata())){NULL}
    else{downloadButton('downloadAssessmentUnits', 'Download')}
  })
  
  
  
  output$Categories2 <- renderUI({
    list(
      tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("Categories")
    )})
}

shinyApp(ui=ui, server=server)
