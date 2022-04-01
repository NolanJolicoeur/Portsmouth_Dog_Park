#Load in Functions
library(shiny)
library(datasets)
library(stringr)
library(tidyr)
library(dplyr)
library(sqldf)

#Code for ui (look of the dashboard)
ui <- shinyUI(fluidPage(
  #Main Page Title and tab to upload files for dog park data and weather data. 
  titlePanel("Dog Park Dashboard"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose Dog Park TXT File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 fileInput('file2', 'Choose Weather TXT File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 plotOutput('contents')
               )
             )
    ),
    #Tab Panel for Daily dog park data breakdown.  Includes y and x-axis selector and graph. 
    tabPanel("Daily",
             pageWithSidebar(
               headerPanel('Choose Plot'),
               sidebarPanel(
                 selectInput('xcol1', 'X Variable 1', ""),
                 selectInput('ycol1', 'Y Variable 1', "", selected = ""),
                 
               ),
               mainPanel(
                 plotOutput('MyPlot1')
               )
             )
    ),
    #Tab Panel for Monthly dog park data breakdown.  Includes y and x-axis selector and graph.
    tabPanel("Monthly",
             pageWithSidebar(
               headerPanel('Choose Plot'),
               sidebarPanel(
                 selectInput('xcol2', 'X Variable 2', ""),
                 selectInput('ycol2', 'Y Variable 2', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot2')
               )
             )
    )
    
  )
)
)
#Sever code (makes the ui work)
server <- shinyServer(function(input, output, session) {
  
  #this code block takes the input data file and converts it into a dataframe for daily breakdown.
  data1 <- reactive({ 
    req(input$file1) 
    
    inFile1 <- input$file1 
    
    df1 <- read.delim(inFile1$datapath, header = input$header, sep = input$sep,
                      quote = input$quote)
    df1 <- read.delim(file.choose())
    df1 <- as.data.frame(df1)
    df1 <- df1[grep('00000',df1$D),]
    df1 <- as.data.frame(df1)
    df1 <- df1 %>% separate(df1, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
    df1$Day <- as.numeric(df1$Day)
    df1$TrafX_Count <- as.numeric(df1$TrafX_Count)
    df1 <- sqldf("select Day, sum(TrafX_Count) from df1 group by Day")
    df1 <- rename(df1, 'TrafX_Count' = 'sum(TrafX_Count)')
    df1$Human_Count <- round((0.3673 * df1$TrafX_Count),0)
    df1$Dog_Count <- round((0.4591 * df1$TrafX_Count),0)
    df1$Car_Count <- round((0.2959 * df1$TrafX_Count),0)
    
    
    
 
    #Labels for selction options
    updateSelectInput(session, inputId = 'xcol1', label = 'X Variable 1',
                      choices = names(df1), selected = names(df1))
    updateSelectInput(session, inputId = 'ycol1', label = 'Y Variable 1',
                      choices = names(df1), selected = names(df1)[2])
    
    
    return(df1)
  })
  #Plot and graph data 
  output$contents <- renderPlot({
    data1()
  })
  
  output$MyPlot1 <- renderPlot({

    x <- data1()[, c(input$xcol1, input$ycol1)]
    plot(x)
    
  })
  #this code block takes the input data file and converts it into a dataframe for monthly breakdown.
  data2 <- reactive({ 
    req(input$file1) 
    
    inFile1a <- input$file1 
    
    df2 <- read.delim(inFile1a$datapath, header = input$header, sep = input$sep,
                      quote = input$quote)
    df2 <- read.delim(file.choose())
    df2 <- as.data.frame(df2)
    df2 <- df2[grep('00000',df2$D),]
    df2 <- as.data.frame(df2)
    df2 <- df2 %>% separate(df2, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
    df2$Month <- as.numeric(df2$Month)
    df2$TrafX_Count <- as.numeric(df2$TrafX_Count)
    df2 <- sqldf("select Month, sum(TrafX_Count) from df2 group by Month")
    df2 <- rename(df2, 'TrafX_Count' = 'sum(TrafX_Count)')
    df2$Human_Count <- round((0.3673 * df2$TrafX_Count),0)
    df2$Dog_Count <- round((0.4591 * df2$TrafX_Count),0)
    df2$Car_Count <- round((0.2959 * df2$TrafX_Count),0)
    
    
    
  
    #Labels for selction options
    updateSelectInput(session, inputId = 'xcol2', label = 'X Variable 2',
                      choices = names(df2), selected = names(df))
    updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable 2',
                      choices = names(df2), selected = names(df2)[2])
    
    return(df2)
  })
  #Plot and graph data
  output$contents <- renderPlot({
    data2()
  })
  
  output$MyPlot2 <- renderPlot({
  
    x <- data2()[, c(input$xcol2, input$ycol2)]
    plot(x)
    
  })
})

shinyApp(ui, server)
