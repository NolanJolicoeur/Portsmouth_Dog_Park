library(shiny)
library(stringr)
library(tidyr)
library(dplyr)
library(sqldf)
library(lubridate)
library(ggplot2)
library(data.table)
library(tidyverse)
library(knitr)


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
               mainPanel(
                 plotOutput('Plot1'),
                 plotOutput('Plot2'),
                 plotOutput('Plot3')
)))))

server <- shinyServer(function(input, output) {
  df1 <- readRDS('Dog_Park_Data_File_04282022.RDS')
  #Data for attendance by time of day
  dfavgtimeofday1 <- sqldf("select Time, Time_of_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from df1 group by Time")
  dfavgtimeofday1 <- rename(dfavgtimeofday1, 'TrafX_Count' = 'avg(TrafX_Count)')
  dfavgtimeofday1<- rename(dfavgtimeofday1, 'Human_Count' = 'avg(Human_Count)')
  dfavgtimeofday1 <- rename(dfavgtimeofday1, 'Dog_Count' = 'avg(Dog_Count)')
  dfavgtimeofday1 <- rename(dfavgtimeofday1, 'Car_Count' = 'avg(Car_Count)')
  dfavgtimeofday <- sqldf("select  Time, Time_of_Day, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from dfavgtimeofday1 group by Time_of_Day")
  dfavgtimeofday <- rename(dfavgtimeofday, 'TrafX_Count' = 'sum(TrafX_Count)')
  dfavgtimeofday <- rename(dfavgtimeofday, 'Human_Count' = 'sum(Human_Count)')
  dfavgtimeofday <- rename(dfavgtimeofday, 'Dog_Count' = 'sum(Dog_Count)')
  dfavgtimeofday <- rename(dfavgtimeofday, 'Car_Count' = 'sum(Car_Count)')
  dfavgtimeofday <- rename(dfavgtimeofday, 'Key' = 'Time_of_Day')
  dfavgtimeofday <- dfavgtimeofday[order(dfavgtimeofday$Time),]
  #Graph 
  dfavgtimeofday_Tall <- dfavgtimeofday %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  dfavgtimeofday_graph <- ggplot(dfavgtimeofday_Tall, aes(Count, Value, fill = Key))+
    labs(title = 'Average Attendance By Time of Day', x = 'Type', y = 'Count', color = 'Key:')+
    geom_col(position = 'dodge')
  output$Plot1 <- renderPlot(dfavgtimeofday_graph)
  
  #Daily data with graphs 
  #Group data by date
  dfdaily <- sqldf('select Date, Day_Name, Day_Type, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df1 group by Date')
  dfdaily <- rename(dfdaily, 'TrafX_Count' = 'sum(TrafX_Count)')
  dfdaily <- rename(dfdaily, 'Human_Count' = 'sum(Human_Count)')
  dfdaily <- rename(dfdaily, 'Dog_Count' = 'sum(Dog_Count)')
  dfdaily <- rename(dfdaily, 'Car_Count' = 'sum(Car_Count)')
  #Graph data by date 
  daily_graph <- ggplot(dfdaily, aes(x = Date, group = 1))+ 
    labs(title = 'Daily Attendance for Dogs, Humans and Cars', x = 'Date', y = 'Count' , color = 'Key:')+
    geom_line(aes(y = Dog_Count), color = 'green')+
    geom_point(aes(y = Dog_Count, color = 'Dogs'))+
    geom_line(aes(y = Human_Count), color = 'blue')+
    geom_point(aes(y = Human_Count, color = 'Humans'))+ 
    geom_line(aes(y = Car_Count), color = 'red')+
    geom_point(aes(y = Car_Count, color = 'Cars'))+
    scale_y_continuous(breaks = scales::breaks_width(50))
  output$Plot2 <- renderPlot(daily_graph)
  
  #Attendance by day of week
  dfavgdaily <- sqldf('select Date, Day_Name, Day_Type, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfdaily group by Day_Name')
  dfavgdaily <- rename(dfavgdaily, 'TrafX_Count' = 'avg(TrafX_Count)')
  dfavgdaily <- rename(dfavgdaily, 'Human_Count' = 'avg(Human_Count)')
  dfavgdaily <- rename(dfavgdaily, 'Dog_Count' = 'avg(Dog_Count)')
  dfavgdaily <- rename(dfavgdaily, 'Car_Count' = 'avg(Car_Count)')
  #Graph
  daily_avg_graph <- ggplot(dfavgdaily, aes(x = factor(Day_Name, weekdays(min(Date)+0:6)), group = 1))+
    labs(title = 'Average Dog, Human, and Car Count by the Day of the Week', x = 'Day of Week', y = 'Count', color = 'Key:')+
    geom_line(aes(y = Dog_Count), color = 'green')+
    geom_point(aes(y = Dog_Count, color = 'Dogs'))+
    geom_line(aes(y = Human_Count), color = 'blue')+
    geom_point(aes(y = Human_Count, color = 'Humans'))+
    geom_line(aes(y = Car_Count), color = 'red')+
    geom_point(aes(y = Car_Count, color = 'Cars'))+
    lims(y = c(0,200))
  output$Plot3 <- renderPlot(daily_avg_graph)
})

shinyApp(ui, server)
