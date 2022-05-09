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
    tabPanel("Daily Summary",
               mainPanel(
                 plotOutput('Plot1'),
                 plotOutput('Plot2'),
                 plotOutput('Plot3')
)),
  tabPanel('Weekend vs Weekday',
              mainPanel(
                plotOutput('Plot4'),
                plotOutput('Plot5')
              )),
  tabPanel('Monthly Summary',
              mainPanel(
                plotOutput('Plot6')
              )),
  tabPanel('Weather Summary',
              mainPanel(
                plotOutput('Plot7'),
                plotOutput('Plot8'),
                plotOutput('Plot9')
              ))
)))

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
  #Group by weekend and weekday
  dfavgDay_Type <- sqldf("select  Day_Type, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfavgdaily group by Day_Type")
  dfavgDay_Type <- rename(dfavgDay_Type, 'TrafX_Count' = 'avg(TrafX_Count)')
  dfavgDay_Type <- rename(dfavgDay_Type, 'Human_Count' = 'avg(Human_Count)')
  dfavgDay_Type <- rename(dfavgDay_Type, 'Dog_Count' = 'avg(Dog_Count)')
  dfavgDay_Type <- rename(dfavgDay_Type, 'Car_Count' = 'avg(Car_Count)')
  dfavgDay_Type <- rename(dfavgDay_Type, 'Key' = 'Day_Type')
  #Graph
  dfavgDay_Type_Tall <- dfavgDay_Type %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  dfavgDay_Type_graph <- ggplot(dfavgDay_Type_Tall, aes(Count, Value, fill = Key))+
    labs(title = 'Average Attendance on Weekends vs Weekdays', x = 'Type', y = 'Count', color = 'Key:')+
    geom_col(position = 'dodge')
  output$Plot4 <- renderPlot(dfavgDay_Type_graph)
  #Create new data frame just for specific columns needed for next graphs
  df2 <- df1
  df2$Date_Time_of_Day <- paste(df2$Date, df2$Time_of_Day)
  #Group by time of day and weekend or weekday
  dfavgDay_Type_Time_of_Day1 <- sqldf("select Day_Type_Time_of_Day, Time_of_Day, Date_Time_of_Day, Date, Time, Day_Type, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df2 group by Date_Time_of_Day")
  dfavgDay_Type_Time_of_Day1 <- rename(dfavgDay_Type_Time_of_Day1, 'TrafX_Count' = 'sum(TrafX_Count)')
  dfavgDay_Type_Time_of_Day1 <- rename(dfavgDay_Type_Time_of_Day1, 'Human_Count' = 'sum(Human_Count)')
  dfavgDay_Type_Time_of_Day1 <- rename(dfavgDay_Type_Time_of_Day1, 'Dog_Count' = 'sum(Dog_Count)')
  dfavgDay_Type_Time_of_Day1 <- rename(dfavgDay_Type_Time_of_Day1, 'Car_Count' = 'sum(Car_Count)')
  dfavgDay_Type_Time_of_Day <- sqldf("select Time_of_Day, Time, Day_Type, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfavgDay_Type_Time_of_Day1 group by Day_Type_Time_of_Day")
  dfavgDay_Type_Time_of_Day <- rename(dfavgDay_Type_Time_of_Day, 'TrafX_Count' = 'avg(TrafX_Count)')
  dfavgDay_Type_Time_of_Day <- rename(dfavgDay_Type_Time_of_Day, 'Human_Count' = 'avg(Human_Count)')
  dfavgDay_Type_Time_of_Day <- rename(dfavgDay_Type_Time_of_Day, 'Dog_Count' = 'avg(Dog_Count)')
  dfavgDay_Type_Time_of_Day <- rename(dfavgDay_Type_Time_of_Day, 'Car_Count' = 'avg(Car_Count)')
  #Graph for Weekends
  dfavgDay_Type_Time_of_Day_Weekend <-sqldf("select Time_of_Day, Time, Day_Type, TrafX_Count, Human_Count, Dog_Count, Car_Count from dfavgDay_Type_Time_of_Day where Day_Type == 'Weekend'")
  dfavgDay_Type_Time_of_Day_Weekend <- rename(dfavgDay_Type_Time_of_Day_Weekend, 'Key' = 'Time_of_Day')
  dfavgDay_Type_Time_of_Day_Weekend_Tall <- dfavgDay_Type_Time_of_Day_Weekend %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  dfavgDay_Type_Time_of_Day_Weekend_Tall_graph <- ggplot(dfavgDay_Type_Time_of_Day_Weekend_Tall, aes(Count, Value, fill = Key))+
    labs(title = 'Average Attendance on Weekends Per Time of Day', x = 'Time', y = 'Count', color = 'Key:')+
    geom_col(position = 'dodge')+
    lims(y = c(0,100))
  #Graph for Weekdays 
  dfavgDay_Type_Time_of_Day_Weekday <-sqldf("select Time_of_Day, Time, Day_Type, TrafX_Count, Human_Count, Dog_Count, Car_Count from dfavgDay_Type_Time_of_Day where Day_Type == 'Weekday'")
  dfavgDay_Type_Time_of_Day_Weekday <- rename(dfavgDay_Type_Time_of_Day_Weekday, 'Key' = 'Time_of_Day')
  dfavgDay_Type_Time_of_Day_Weekday_Tall <- dfavgDay_Type_Time_of_Day_Weekday %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  dfavgDay_Type_Time_of_Day_Weekday_Tall_graph <- ggplot(dfavgDay_Type_Time_of_Day_Weekday_Tall, aes(Count, Value, fill = Key))+
    labs(title = 'Average Attendance on Weekdays Per Time of Day', x = 'Time', y = 'Count', color = 'Key:')+
    geom_col(position = 'dodge')+
    lims(y = c(0,100))
  output$Plot5 <- renderPlot(grid.arrange(dfavgDay_Type_Time_of_Day_Weekend_Tall_graph, dfavgDay_Type_Time_of_Day_Weekday_Tall_graph, ncol=2))
  #Attendance by Month
  attendance_by_month <- sqldf("select Date, Month, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df1 group by Month")
  attendance_by_month <- rename(attendance_by_month, 'TrafX_Count' = 'sum(TrafX_Count)')
  attendance_by_month <- rename(attendance_by_month, 'Human_Count' = 'sum(Human_Count)')
  attendance_by_month <- rename(attendance_by_month, 'Dog_Count' = 'sum(Dog_Count)')
  attendance_by_month <- rename(attendance_by_month, 'Car_Count' = 'sum(Car_Count)')
  #Graph 
  attendance_by_month_graph <- ggplot(attendance_by_month, aes(Month, group = 1))+
    labs(title = 'Total Attendance by the Month', x = 'Month', y = 'Total Attendance', color = 'Key:')+
    geom_line(aes(y = Dog_Count), color = 'green')+
    geom_point(aes(y = Dog_Count, color = 'Dogs'))+
    geom_line(aes(y = Human_Count), color = 'blue')+
    geom_point(aes(y = Human_Count, color = 'Humans'))+ 
    geom_line(aes(y = Car_Count), color = 'red')+
    geom_point(aes(y = Car_Count, color = 'Cars'))
  output$Plot6 <- renderPlot(attendance_by_month_graph)
  dfdailyweather <- sqldf("select Date, avg(Temp), sum(Precipitation), avg(Wind), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df1 group by Date")
  dfdailyweather <- rename(dfdailyweather, 'Temp' = 'avg(Temp)')
  dfdailyweather <- rename(dfdailyweather, 'Precipitation' = 'sum(Precipitation)')
  dfdailyweather <- rename(dfdailyweather, 'Wind' = 'avg(Wind)')
  dfdailyweather <- rename(dfdailyweather, 'Human_Count' = 'sum(Human_Count)')
  dfdailyweather <- rename(dfdailyweather, 'Dog_Count' = 'sum(Dog_Count)')
  dfdailyweather <- rename(dfdailyweather, 'Car_Count' = 'sum(Car_Count)')
  dfdailyweather$Temp_Range <- with(dfdailyweather, ifelse(Temp <= 20, '1- (< 20 Degrees)', ifelse(Temp <= 40, '2- (20-40 Degrees)', ifelse(Temp <= 60, '3- (40-60 Degrees)', ifelse(Temp <= 80, '4- (60-80 Degrees)', ifelse(Temp <= 10000, '5- (> 80 Degrees)'))))))
  dfdailyweather$Precipitation_Range <- with(dfdailyweather, ifelse(Precipitation <= 0.0000001, '1- No Precipitation', ifelse(Precipitation <= 0.25, '2- Light Precipitation', ifelse(Precipitation <= 0.5, '3- Consistent Precipitation', ifelse(Precipitation <= 0.75, '4- Consisten Heavy Precipitation', ifelse(Precipitation <= 80, '5- Storm Precipitation'))))))
  dfdailyweather$Wind_Range <- with(dfdailyweather, ifelse(Wind <= 1, '1- No Wind', ifelse(Wind <= 6, '2- Light Winds', ifelse(Wind <= 15, '3- Moderate Winds', ifelse(Wind <= 30, '4- Strong Winds', ifelse(Wind <= 50, '5- Storm Winds', ifelse(Wind <= 1000, '6- Violent Winds')))))))
  dfdailytemp <- sqldf("select Temp_Range, avg(Temp), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfdailyweather group by Temp_Range")
  dfdailytemp <- rename(dfdailytemp, 'Temp' = 'avg(Temp)')
  dfdailytemp <- rename(dfdailytemp, 'Human_Count' = 'avg(Human_Count)')
  dfdailytemp <- rename(dfdailytemp, 'Dog_Count' = 'avg(Dog_Count)')
  dfdailytemp <- rename(dfdailytemp, 'Car_Count' = 'avg(Car_Count)')
  dfdailytemp <- rename(dfdailytemp, 'Key' = 'Temp_Range')
  attendancebytempgraph_tall <- dfdailytemp %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  attendancebytempgraph <- ggplot(attendancebytempgraph_tall, aes(Count, Value, fill = Key))+
    labs(title = 'Attendance by Average Daily Temperature', x = 'Type', y = 'Attendance', color = 'Key:')+
    geom_col(position = 'dodge')
  output$Plot7 <- renderPlot(attendancebytempgraph)
  # Daily Attendance by Precipitation 
  dfdailyprecipitation <- sqldf("select Precipitation_Range, avg(Precipitation), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfdailyweather group by Precipitation_Range")
  dfdailyprecipitation <- rename(dfdailyprecipitation, 'Precipitation' = 'avg(Precipitation)')
  dfdailyprecipitation <- rename(dfdailyprecipitation, 'Human_Count' = 'avg(Human_Count)')
  dfdailyprecipitation <- rename(dfdailyprecipitation, 'Dog_Count' = 'avg(Dog_Count)')
  dfdailyprecipitation <- rename(dfdailyprecipitation, 'Car_Count' = 'avg(Car_Count)')
  dfdailyprecipitation <- rename(dfdailyprecipitation, 'Key' = 'Precipitation_Range')
  attendancebyprecipitationgraph_tall <- dfdailyprecipitation %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  attendancebyprecipitationgraph <- ggplot(attendancebyprecipitationgraph_tall, aes(Count, Value, fill = Key))+
    labs(title = 'Attendance by Average Daily Precipitation-Measurements from Weather Insurance Agency', x = 'Type', y = 'Attendance', color = 'Key:')+
    geom_col(position = 'dodge')
  output$Plot8 <- renderPlot(attendancebyprecipitationgraph)
  # Daily Attendance by Wind
  dfdailywind <- sqldf("select Wind_Range, avg(Wind), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfdailyweather group by Wind_Range")
  dfdailywind <- rename(dfdailywind, 'Wind' = 'avg(Wind)')
  dfdailywind <- rename(dfdailywind, 'Human_Count' = 'avg(Human_Count)')
  dfdailywind <- rename(dfdailywind, 'Dog_Count' = 'avg(Dog_Count)')
  dfdailywind <- rename(dfdailywind, 'Car_Count' = 'avg(Car_Count)')
  dfdailywind <- rename(dfdailywind, 'Key' = 'Wind_Range')
  attendancebywindgraph_tall <- dfdailywind %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
  attendancebywindgraph <- ggplot(attendancebywindgraph_tall, aes(Count, Value, fill = Key))+
    labs(title = 'Attendance by Average Daily Wind-Measurements from Beaufort Wind Scale', x = 'Type', y = 'Attendance', color = 'Key:')+
    geom_col(position = 'dodge')
  output$Plot9 <- renderPlot(attendancebywindgraph)
})

shinyApp(ui, server)
