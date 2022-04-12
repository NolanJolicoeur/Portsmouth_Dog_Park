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
    df1 <- sqldf("select Year, Day, Month, Time, sum(TrafX_Count) from df1 group by Day")
    df1 <- rename(df1, 'TrafX_Count' = 'sum(TrafX_Count)')
    df1$Human_Count <- round((0.3673 * df1$TrafX_Count),0)
    df1$Dog_Count <- round((0.4591 * df1$TrafX_Count),0)
    df1$Car_Count <- round((0.2959 * df1$TrafX_Count),0)
  #this code block takes the input data file and converts it into a dataframe for daily breakdown.
})
  
  output$myplot1 <- renderPlot({
    ggplot(dfavghourly, aes(x = Time, group = 1))+
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

