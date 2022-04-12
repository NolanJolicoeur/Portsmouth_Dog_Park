#Load in Functions
library(shiny)
library(datasets)
library(stringr)
library(tidyr)
library(dplyr)
library(sqldf)
library(ggplot2)

#Code for ui (look of the dashboard)
ui <- shinyUI(fluidPage(
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
                 plotOutput('myplot1'
               )
  )
  )
    )
  ),
)
)

    
    
  


#Sever code (makes the ui work)
server <- shinyServer(function(input, output, session) {
  #How do I make a sepparate data frame based off this??
  data1 <- reactive({ 
    req(input$file1) 
    
    inFile1 <- input$file1 
    
    df5 <- read.delim(inFile1$datapath, header = input$header, sep = input$sep,
                      quote = input$quote)
    df5 <- read.delim(file.choose())
    df5 <- as.data.frame(df5)
    df5 <- df1[grep('00000',df5$D),]
    df5 <- as.data.frame(df5)
    df5 <- df5 %>% separate(df5, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
    df5$Day <- as.numeric(df5$Day)
    df5$TrafX_Count <- as.numeric(df5$TrafX_Count)
    df5 <- sqldf("select Year, Day, Month, Time, sum(TrafX_Count) from df5")
    df5 <- rename(df5, 'TrafX_Count' = 'sum(TrafX_Count)')
    df5$Human_Count <- round((0.3673 * df5$TrafX_Count),0)
    df5$Dog_Count <- round((0.4591 * df5$TrafX_Count),0)
    df5$Car_Count <- round((0.2959 * df5$TrafX_Count),0)
  #this code block takes the input data file and converts it into a dataframe for daily breakdown.
})
  
  output$myplot1 <- renderPlot({
    ggplot(df5, aes(x = Time, group = 1))+
      labs(title = 'Average Dog, Human, and Car Count by the Hour', x = 'Time', y = 'Count', color = 'Key:')+
      geom_line(aes(y = Dog_Count), color = 'green')+
      geom_point(aes(y = Dog_Count, color = 'Dogs'))+
      geom_line(aes(y = Human_Count), color = 'blue')+
      geom_point(aes(y = Human_Count, color = 'Humans'))+
      geom_line(aes(y = Car_Count), color = 'red')+
      geom_point(aes(y = Car_Count, color = 'Cars'))+
      scale_y_continuous(breaks = scales::breaks_width(2))
  })
})

shinyApp(ui, server)
