#Load in all the necessary packages.
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
library(kableExtra)
library(gridExtra)
library(forcats)
library(RSQLite)
library(rdrop2)
library(googlesheets4)
#grant access to the google sheets which the data is being uploaded to and pulled from.
gs4_auth(cache = "1S4IM2z4yeALESsEQ3WmcTBxWubonVxs8htxbeXuLnyE", email = "nolanjoli@gmail.com")
gs4_auth(cache = "1PFyV6ZRtVZwAeL2Yd7U612iIdHUEx4QQ1MUM_oBoM48", email = "nolanjoli@gmail.com")

#The UI is the format for the visual components of the dashboard. 
ui <- shinyUI(fluidPage(
  #Main Page Title and tab to upload files for dog park data and weather data. 
  titlePanel("Dog Park Dashboard"),
  tabsetPanel(
    tabPanel("File Upload/Quick Glance",
             fluidRow(
               column(3, fileInput('file1', 'Choose File',
                                   accept=c('text/csv', 'text/comma-separated- values,text/plain', '.txt', '.csv'), multiple = T),
                      textOutput('recent'),
                      textOutput('RecentData1'),
                      textOutput('RecentData'),
                      actionButton('Click1', 'Add Data!'),
                      checkboxInput("header", "Header", T)
               ),
               
               # Manually selected date range 
               column(9, textOutput('FileNames'),
                      column(3, textInput('Text2', 'Starting Month'),
                             textInput('Text5', 'Ending Month')),
                      column(3, textInput('Text3', 'Starting Day'), 
                             textInput('Text6', 'Ending Day')),
                      column(3, textInput('Text1', 'Starting Year'),
                             textInput('Text4', 'Ending Year'),
                             actionButton('Click', 'Run Data!'))),
             ),
             #Overview of data for dashboard
             titlePanel('Portsmouth Dog Park at a Glance'),
             textOutput('Label1'),
             textOutput('Range1'),
             fluidRow(
               column(3, tableOutput('Table5')),
               column(3, tableOutput('Table6')),
               column(3, tableOutput('Table7')),
               column(3, tableOutput('Table8'))
             ),
             plotOutput('Plot14'),
             plotOutput('Plot15'),
             plotOutput('Plot16'),
             plotOutput('Plot17')),
    
    #Daily data tab
    tabPanel("Daily Summary",
             mainPanel(
               textOutput('Label2'),
               textOutput('Range2'),
               plotOutput('Plot1'),
               tableOutput('Table1'),
               plotOutput('Plot2'),
               tableOutput('Table2'),
               plotOutput('Plot3')
             )),
    
    #Weekend vs Weekday tab
    tabPanel('Weekend vs Weekday',
             mainPanel(
               textOutput('Label3'),
               textOutput('Range3'),
               plotOutput('Plot4'),
               tableOutput('Table4'),
               plotOutput('Plot5')
             )),
    
    #Monthly and Seasonal data tab
    tabPanel('Monthly/Seasonal Summary',
             mainPanel(
               textOutput('Label4'),
               textOutput('Range4'),
               plotOutput('Plot6'),
               tableOutput('Table3'),
               plotOutput('Plot18'),
               tableOutput('Table9')
             )),
    
    #Weather data tab
    tabPanel('Weather Summary',
             mainPanel(
               textOutput('Label5'),
               textOutput('Range5'),
               plotOutput('Plot7'),
               plotOutput('Plot8'),
               plotOutput('Plot9'),
               plotOutput('Plot11'),
               plotOutput('Plot12'),
               plotOutput('Plot13')
             )),
    
    #Waste bag tab 
    tabPanel('Waste Bags',
             plotOutput('Plot10'),
             mainPanel(
               column(4,textInput('Text7', 'Waste Boxes Emptied')),
               column(4, textInput('Text8', 'Month')),
               column(4, textInput('Text9', 'Year')),
               column(12, "Input data as (two digits - two digits - four digits)"),
               column(12, actionButton('Click2', 'Add Data!'))
             ))
  )))

#The server is the componenet of the dashboard behind the scenes that makes everythin run. 
#R-Shiny is reactive so it requires buttons or inputs to trigger graphs or any other visuals to appear. 
server <- shinyServer(function(input, output) {
  outputDir <- "Data"

  #This is a function that takes the data from the shuttle files and configures it into a readable data frame. 
  process <- function(data1){
    df <- as.data.frame(data1)
    df <- df[grep('00000',df$D),]
    df <- as.data.frame(df)
    df <- df %>% separate(df, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
    df <- sqldf("select Year, Month, Day, Time, TrafX_Count from df")
    df$TrafX_Count <- as.numeric(df$TrafX_Count)
    df$Date_Time <- paste( df$Year, df$Month, df$Day, df$Time)
    df$Date <- paste(df$Month,'-', df$Day,'-', df$Year)
    df <- sqldf("select Year, Month, Day, Time,sum(TrafX_Count), Date_Time, Date from df group by Date_Time")
    df <- rename(df, 'TrafX_Count' = 'sum(TrafX_Count)')
    return(df)}
  
  #This code chunk takes the data from the shuttle file and uploades it into the google sheet
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1S4IM2z4yeALESsEQ3WmcTBxWubonVxs8htxbeXuLnyE/edit#gid=1152415696")
  output$recent <- renderText('Date Range of Data Uploaded:')
  recentdate <- tail(df$Date, 1)
  firstdate <- head(df$Date, 1)
  output$RecentData <- renderText(recentdate)
  output$RecentData1 <- renderText(firstdate)
  observeEvent(input$Click1,{
    df <- read_sheet("https://docs.google.com/spreadsheets/d/1S4IM2z4yeALESsEQ3WmcTBxWubonVxs8htxbeXuLnyE/edit#gid=1152415696")
    file <- input$file1
    NewFile <- read.delim(file$datapath)
    NewFile <- process(NewFile)
    NewFile <- sqldf("
               SELECT * FROM df
               UNION
               SELECT * FROM NewFile
               ")
    NewFile <- NewFile %>% distinct()
    sheet_write(NewFile, "1S4IM2z4yeALESsEQ3WmcTBxWubonVxs8htxbeXuLnyE", sheet = 1)
    recentdate <- tail(NewFile$Date, 1)
    output$RecentData <- renderText(recentdate)
  })
  
  
  #This code pulls data from within a certain time frame. 
  observeEvent(input$Click, {
    year1 <- input$Text1
    month1 <- input$Text2
    day1 <- input$Text3 
    year2 <- input$Text4 
    month2 <- input$Text5 
    day2 <- input$Text6
    range <- paste(month1, '-', day1, '-', year1, ' / ', month2, '-', day2, '-', year2)
    output$Label1 <- renderText('User Inputted Date Range:')
    output$Range1 <- renderText(range)
    output$Label2 <- renderText('User Inputted Date Range:')
    output$Range2 <- renderText(range)
    output$Label3 <- renderText('User Inputted Date Range:')
    output$Range3 <- renderText(range)
    output$Label4 <- renderText('User Inputted Date Range:')
    output$Range4 <- renderText(range)
    output$Label5 <- renderText('User Inputted Date Range:')
    output$Range5 <- renderText(range)
    
    #This code pulls the data from the manual dog counts and organizes it to find ratios. 
    df1 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQeQ0YiEBVASJN0EukDk3Y6CB8rf9aaQJ-d0ViW_nWYf_3gsHHcYu87eKQIgzFOpM_mV9stuA75x0zw/pub?gid=0&single=true&output=csv")
    df1$Day_Name <- weekdays(df1$Date, abbreviate = FALSE)
    weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    df1$Day_Type <- factor((weekdays(df1$Date, abbreviate = FALSE) %in% weekdays1), 
                           levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
    df1$People_Ratio <- df1$People/df1$TrafX_Count
    df1$Dog_Ratio <- df1$Dogs/df1$TrafX_Count
    df1$Cars_Ratio <- df1$Cars/df1$TrafX_Count
    df1$Error <- (df1$Actual_Total-df1$TrafX_Count)/df1$TrafX_Count
    people_to_count_ratio <- (sum(df1$People))/(sum(df1$TrafX_Count))
    dogs_to_count_ratio <- (sum(df1$Dogs))/(sum(df1$TrafX_Count))
    cars_to_count_ratio <- (sum(df1$Cars))/(sum(df1$TrafX_Count))
    ratio_table <- matrix(c(people_to_count_ratio, dogs_to_count_ratio, cars_to_count_ratio), nrow = 3)
    colnames(ratio_table) <- c('Ratio')
    rownames(ratio_table) <- c('People', 'Dogs', 'Cars')
    
    
    #This code reads the data from the shuttle file spreadsheet and organizes it with more columns.  
    #It also applies the ratios from the previous code chunk. 
    df3 <- read_sheet("https://docs.google.com/spreadsheets/d/1S4IM2z4yeALESsEQ3WmcTBxWubonVxs8htxbeXuLnyE/edit#gid=1152415696")
    df3 <- sqldf("select Year, Month, Day, Time, TrafX_Count from df3")
    df3$Human_Count <- round(((sum(df1$People)/sum(df1$TrafX_Count)) * df3$TrafX_Count),0)
    df3$Dog_Count <- round(((sum(df1$Dogs)/sum(df1$TrafX_Count)) * df3$TrafX_Count),0)
    df3$Car_Count <- round(((sum(df1$Cars)/sum(df1$TrafX_Count)) * df3$TrafX_Count),0)
    df3$Year <- str_glue("20{df3$Year}")
    df3$Day <- str_glue("-{df3$Day}")
    df3$Month <- str_glue("-{df3$Month}")
    df3$Date <- paste( df3$Year, df3$Month, df3$Day)
    df3<-df3 %>% mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
    df3$Date <- as.Date(df3$Date)
    df3$Day_Name <- weekdays(df3$Date)
    df3<-df3 %>% mutate(across(where(is.character), str_remove_all, pattern = fixed("-")))
    weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    df3$Day_Type <- factor((weekdays(df3$Date) %in% weekdays1), 
                           levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
    df3$Time <- as.numeric(df3$Time)
    df3$Time_of_Day <- with(df3, ifelse(Time <= 3, '1- (12AM - 2AM)', ifelse(Time <= 6, '2- (3AM - 5AM)', ifelse(Time <= 9, '3- (6AM - 8AM)', ifelse(Time <= 12, '4- (9AM - 11AM)', ifelse(Time <= 15, '5- (12PM - 2PM)', ifelse(Time <= 18, '6- (3PM - 5PM)', ifelse(Time <= 21, '7- (6PM - 8PM)', ifelse(Time <= 24, '8- (9PM - 11PM)')))))))))
    df3$Time_Official <- paste(df3$Date, df3$Time)  
    df3 <- sqldf("select Year, Month, Day, Time, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count), Date, Day_Name, Day_Type, Time_of_Day, Time_Official from df3 group by Time_Official")
    df3 <- rename(df3, 'TrafX_Count' = 'sum(TrafX_Count)')
    df3 <- rename(df3, 'Human_Count' = 'sum(Human_Count)')
    df3 <- rename(df3, 'Dog_Count' = 'sum(Dog_Count)')
    df3 <- rename(df3, 'Car_Count' = 'sum(Car_Count)')
    df3$Time_Official <- NULL
    df3$Day_Type_Time_of_Day <- paste(df3$Day_Type, df3$Time_of_Day)
    df3 <- df3[order(df3$Date, df3$Time),]
    df3 <- df3 %>% mutate(Week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% arrange(Date)
    df3$Date_Time <- paste(df3$Date, df3$Time)
    
    #This creates the data frame for weather data 
    url = glue::glue("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=UUU&data=all&year1={year1}&month1={month1}&day1={day1}&year2={year2}&month2={month2}&day2={day2}&tz=Etc%2FUTC&format=onlycomma&latlon=no&elev=no&missing=M&trace=T&direct=no&report_type=1&report_type=2")
    df2 <- read.csv(url)
    df2 <- df2[df2$tmpf != 'M',]
    df2$valid <- gsub("-"," ",df2$valid, fixed = TRUE)
    df2$valid <- gsub(":"," ",df2$valid, fixed = TRUE)
    df2 <- df2 %>% separate(valid, c('Year','Month','Day', 'Time','Minute'),' ') 
    df2$Day_Time <- paste(df2$Day, df2$Time, df2$Month, df2$Year)
    df2 <- sqldf("select * from df2 group by Day_Time")
    df2$Day <- str_glue("-{df2$Day}")
    df2$Month <- str_glue("-{df2$Month}")
    df2$Date <- paste( df2$Year, df2$Month, df2$Day)
    df2 <-df2 %>% mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
    df2$Date <- as.Date(df2$Date)
    df2 <-df2 %>% mutate(across(where(is.character), str_remove_all, pattern = fixed("-")))
    df2 <- df2[c('Date', 'Year', 'Month', 'Day', 'Time', 'tmpf', 'p01i', 'gust')]
    df2 <- rename(df2, 'Temp' = 'tmpf')
    df2 <- rename(df2, 'Precipitation' = 'p01i')
    df2 <- rename(df2, 'Wind' = 'gust')
    df2$Date1 <- paste(df2$Month, '-', df2$Year)
    df2$Time <- as.numeric(df2$Time)
    df2$Date_Time <- paste(df2$Date, df2$Time)
    
    #Join data frames
    df4 <- sqldf("select * from df2
             LEFT JOIN df3
             ON df2.Date_Time = df3.Date_Time")
    df4 <- df4 %>% drop_na()
    duplicates <- duplicated(colnames(df4))
    df4 <- df4[!duplicates]
    df4 <- df4 %>% distinct()
    #Data for attendance by time of day
    dfavgtimeofday1 <- sqldf("select Time, Time_of_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from df4 group by Time")
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
      theme(text = element_text(size = 15))+
      geom_col(position = 'dodge')
    output$Plot1 <- renderPlot(dfavgtimeofday_graph)
    #table
    dfavgtimeofdaycarsummary <- c(quantile(dfavgtimeofday$Car_Count), mean(dfavgtimeofday$Car_Count))
    dfavgtimeofdaydogsummary <- c(quantile(dfavgtimeofday$Dog_Count), mean(dfavgtimeofday$Dog_Count))
    dfavgtimeofdayhumansummary <- c(quantile(dfavgtimeofday$Human_Count), mean(dfavgtimeofday$Human_Count))
    fivenumbersummarytimeofdaydf <- matrix(c( dfavgtimeofdaycarsummary, dfavgtimeofdaydogsummary, dfavgtimeofdayhumansummary), ncol = 3, nrow = 6)
    rownames(fivenumbersummarytimeofdaydf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(fivenumbersummarytimeofdaydf) <- c('Cars', 'Dogs', 'Humans')
    output$Table1 <- renderTable(fivenumbersummarytimeofdaydf, rownames = T,colnames = T)
    
    #Daily data with graphs 
    #Group data by date
    dfdaily <- sqldf('select Date, Day_Name, Day_Type, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df4 group by Date')
    dfdaily <- rename(dfdaily, 'TrafX_Count' = 'sum(TrafX_Count)')
    dfdaily <- rename(dfdaily, 'Human_Count' = 'sum(Human_Count)')
    dfdaily <- rename(dfdaily, 'Dog_Count' = 'sum(Dog_Count)')
    dfdaily <- rename(dfdaily, 'Car_Count' = 'sum(Car_Count)')
    #Graph data by date 
    daily_graph <- ggplot(dfdaily, aes(x = Date, group = 1))+ 
      labs(title = 'Daily Attendance for Dogs, Humans and Cars', x = 'Date', y = 'Count' , color = 'Key:')+
      theme(text = element_text(size = 15))+
      geom_line(aes(y = Dog_Count), color = 'green')+
      geom_point(aes(y = Dog_Count, color = 'Dogs'))+
      geom_line(aes(y = Human_Count), color = 'blue')+
      geom_point(aes(y = Human_Count, color = 'Humans'))+ 
      geom_line(aes(y = Car_Count), color = 'red')+
      geom_point(aes(y = Car_Count, color = 'Cars'))
    daily_graph1 <- ggplot(dfdaily, aes(x = Date, group = 1))+ 
      labs(title = 'Daily Attendance for Dogs, Humans and Cars', x = 'Date', y = 'Count' , color = 'Key:')+
      theme(legend.position = 'top', text = element_text(size = 15))+
      geom_line(aes(y = Dog_Count), color = 'green')+
      geom_point(aes(y = Dog_Count, color = 'Dogs'))+
      geom_line(aes(y = Human_Count), color = 'blue')+
      geom_point(aes(y = Human_Count, color = 'Humans'))+ 
      geom_line(aes(y = Car_Count), color = 'red')+
      geom_point(aes(y = Car_Count, color = 'Cars'))
    #scale_y_continuous(breaks = scales::breaks_width(50))
    output$Plot2 <- renderPlot(daily_graph)
    output$Plot14 <- renderPlot(daily_graph1)
    #Table
    dfdailycarsummary <- c(quantile(dfdaily$Car_Count), mean(dfdaily$Car_Count))
    dfdailydogsummary <- c(quantile(dfdaily$Dog_Count), mean(dfdaily$Dog_Count))
    dfdailyhumansummary <- c(quantile(dfdaily$Human_Count), mean(dfdaily$Human_Count))
    fivenumbersummarydailydf <- matrix(c( dfdailycarsummary, dfdailydogsummary, dfdailyhumansummary), ncol = 3, nrow = 6)
    rownames(fivenumbersummarydailydf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(fivenumbersummarydailydf) <- c('Cars', 'Dogs', 'Humans')
    output$Table2 <- renderTable(fivenumbersummarydailydf, rownames = T, colnames = T)
    output$Table5 <- renderTable(fivenumbersummarydailydf, rownames = T, colnames = T)
    #Attendance by day of week
    dfavgdaily <- sqldf('select Date, Day_Name, Day_Type, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfdaily group by Day_Name')
    dfavgdaily <- rename(dfavgdaily, 'TrafX_Count' = 'avg(TrafX_Count)')
    dfavgdaily <- rename(dfavgdaily, 'Human_Count' = 'avg(Human_Count)')
    dfavgdaily <- rename(dfavgdaily, 'Dog_Count' = 'avg(Dog_Count)')
    dfavgdaily <- rename(dfavgdaily, 'Car_Count' = 'avg(Car_Count)')
    #Graph
    daily_avg_graph <- ggplot(dfavgdaily, aes(x = factor(Day_Name, weekdays(min(Date)+0:6)), group = 1))+
      labs(title = 'Average Dog, Human, and Car Count by the Day of the Week', x = 'Day of Week', y = 'Count', color = 'Key:')+
      theme(text = element_text(size = 15))+
      geom_line(aes(y = Dog_Count), color = 'green')+
      geom_point(aes(y = Dog_Count, color = 'Dogs'))+
      geom_line(aes(y = Human_Count), color = 'blue')+
      geom_point(aes(y = Human_Count, color = 'Humans'))+
      geom_line(aes(y = Car_Count), color = 'red')+
      geom_point(aes(y = Car_Count, color = 'Cars'))
    #lims(y = c(0,200))
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
      theme(text = element_text(size = 15))+
      geom_col(position = 'dodge')
    output$Plot4 <- renderPlot(dfavgDay_Type_graph)
    #Table 
    daydf <- sqldf("select sum(Car_Count), sum(Dog_Count), sum(Human_Count), Day_Type from df4 group by Date")
    daydf <- rename(daydf, 'Car_Count' = 'sum(Car_Count)')
    daydf <- rename(daydf, 'Dog_Count' = 'sum(Dog_Count)')
    daydf <- rename(daydf, 'Human_Count' = 'sum(Human_Count)')
    Weekenddf <- sqldf("select * from daydf where Day_Type = 'Weekend'")
    Weekdaydf <- sqldf("select * from daydf where Day_Type = 'Weekday'")
    Weekdaydfcarsummary <- c( quantile(Weekdaydf$Car_Count), mean(Weekdaydf$Car_Count))
    Weekenddfcarsummary <- c( quantile(Weekenddf$Car_Count), mean(Weekenddf$Car_Count))
    Weekdaydfdogsummary <- c( quantile(Weekdaydf$Dog_Count), mean(Weekdaydf$Dog_Count))
    Weekenddfdogsummary <- c( quantile(Weekenddf$Dog_Count), mean(Weekenddf$Dog_Count))
    Weekdaydfhumansummary <- c( quantile(Weekdaydf$Human_Count), mean(Weekdaydf$Human_Count))
    Weekenddfhumansummary <- c( quantile(Weekenddf$Human_Count), mean(Weekenddf$Human_Count))
    fivenumbersummarydaytypedf <- matrix(c( Weekdaydfcarsummary, Weekenddfcarsummary, Weekdaydfdogsummary, Weekenddfdogsummary, Weekdaydfhumansummary, Weekenddfhumansummary), ncol = 6, nrow = 6)
    rownames(fivenumbersummarydaytypedf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(fivenumbersummarydaytypedf) <- c('Cars (Weekdays)', 'Cars (Weekend)', 'Dogs (Weekdays)', 'Dogs (Weekend)', 'Humans (Weekdays)', 'Humans (Weekend)')
    output$Table4 <- renderTable(fivenumbersummarydaytypedf, rownames = T, colnames = T)
    
    #Create new data frame just for specific columns needed for next graphs
    df2 <- df4
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
      theme(text = element_text(size = 15))+
      geom_col(position = 'dodge')+
      lims(y = c(0,100))
    #Graph for Weekdays 
    dfavgDay_Type_Time_of_Day_Weekday <-sqldf("select Time_of_Day, Time, Day_Type, TrafX_Count, Human_Count, Dog_Count, Car_Count from dfavgDay_Type_Time_of_Day where Day_Type == 'Weekday'")
    dfavgDay_Type_Time_of_Day_Weekday <- rename(dfavgDay_Type_Time_of_Day_Weekday, 'Key' = 'Time_of_Day')
    dfavgDay_Type_Time_of_Day_Weekday_Tall <- dfavgDay_Type_Time_of_Day_Weekday %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
    dfavgDay_Type_Time_of_Day_Weekday_Tall_graph <- ggplot(dfavgDay_Type_Time_of_Day_Weekday_Tall, aes(Count, Value, fill = Key))+
      labs(title = 'Average Attendance on Weekdays Per Time of Day', x = 'Time', y = 'Count', color = 'Key:')+
      theme(text = element_text(size = 15))+
      geom_col(position = 'dodge')+
      lims(y = c(0,100))
    output$Plot5 <- renderPlot(grid.arrange(dfavgDay_Type_Time_of_Day_Weekend_Tall_graph, dfavgDay_Type_Time_of_Day_Weekday_Tall_graph, ncol=2))
    #Attendance by Month
    attendance_by_month <- sqldf("select Date, Month, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df4 group by Month")
    attendance_by_month <- rename(attendance_by_month, 'TrafX_Count' = 'sum(TrafX_Count)')
    attendance_by_month <- rename(attendance_by_month, 'Human_Count' = 'sum(Human_Count)')
    attendance_by_month <- rename(attendance_by_month, 'Dog_Count' = 'sum(Dog_Count)')
    attendance_by_month <- rename(attendance_by_month, 'Car_Count' = 'sum(Car_Count)')
    #Graph 
    attendance_by_month_graph <- ggplot(attendance_by_month, aes(Month, group = 1))+
      labs(title = 'Total Attendance by the Month', x = 'Month', y = 'Total Attendance', color = 'Key:')+
      theme(text = element_text(size = 15))+
      geom_line(aes(y = Dog_Count), color = 'green')+
      geom_point(aes(y = Dog_Count, color = 'Dogs'))+
      geom_line(aes(y = Human_Count), color = 'blue')+
      geom_point(aes(y = Human_Count, color = 'Humans'))+ 
      geom_line(aes(y = Car_Count), color = 'red')+
      geom_point(aes(y = Car_Count, color = 'Cars'))
    output$Plot6 <- renderPlot(attendance_by_month_graph)
    dfmonthlycarsummary <- c(quantile(attendance_by_month$Car_Count), mean(attendance_by_month$Car_Count)) 
    dfmonthlydogsummary <- c(quantile(attendance_by_month$Dog_Count), mean(attendance_by_month$Dog_Count))
    dfmonthlyhumansummary <- c(quantile(attendance_by_month$Human_Count), mean(attendance_by_month$Human_Count))
    monthlydf <- matrix(c(dfmonthlycarsummary, dfmonthlydogsummary, dfmonthlyhumansummary), ncol = 3, nrow = 6)
    rownames(monthlydf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(monthlydf) <- c('Cars','Dogs', 'Humans')
    output$Table3 <- renderTable(monthlydf, rownames = T, colnames = T)
    
    #Seasonal Data
    Seasonal_Attendance <- sqldf("select Year, Month, Human_Count, Dog_Count, Car_Count from df4")
    Seasonal_Attendance$Month <- as.numeric(Seasonal_Attendance$Month)
    Seasonal_Attendance$Season <- with(Seasonal_Attendance, ifelse(Month <= 2, '1. Winter', ifelse(Month <= 5, '2. Spring', ifelse(Month <= 8, '3. Summer', ifelse(Month <= 11, '4. Fall', ifelse(Month <= 13, '1. Winter'))))))
    Seasonal_Attendance$YearSeason <- paste(Seasonal_Attendance$Year, Seasonal_Attendance$Season)
    
    seasonaldf1 <- sqldf("select Year, Season, sum(Human_Count), sum(Dog_Count), sum(Car_Count) from Seasonal_Attendance group by YearSeason")
    seasonaldf1 <- rename(seasonaldf1, 'Human_Count' = 'sum(Human_Count)')
    seasonaldf1 <- rename(seasonaldf1, 'Dog_Count' = 'sum(Dog_Count)')
    seasonaldf1 <- rename(seasonaldf1, 'Car_Count' = 'sum(Car_Count)')
    
    seasonaldf <- sqldf("select Season, avg(Human_Count), avg(Dog_Count), avg(Car_Count) from seasonaldf1 group by Season")
    seasonaldf <- rename(seasonaldf, 'Human_Count' = 'avg(Human_Count)')
    seasonaldf <- rename(seasonaldf, 'Dog_Count' = 'avg(Dog_Count)')
    seasonaldf <- rename(seasonaldf, 'Car_Count' = 'avg(Car_Count)')
    seasonaldf <- rename(seasonaldf, 'Key' = 'Season')
    
    seasonaldf_tall <- seasonaldf %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
    seasonalgraph <- ggplot(seasonaldf_tall, aes(Count, Value, fill = Key))+
      labs(title = 'Average Attendance by Season', x = 'Season', y = 'Attendance', color = 'Key:')+
      theme(text = element_text(size = 15))+
      geom_col(position = 'dodge')
    output$Plot18 <- renderPlot(seasonalgraph)
    
    #Weather
    dfdailyweather <- sqldf("select Date, avg(Temp), max(Temp), min(Temp), sum(Precipitation), avg(Wind), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df4 group by Date")
    dfdailyweather <- rename(dfdailyweather, 'Temp' = 'avg(Temp)')
    dfdailyweather <- rename(dfdailyweather, 'Precipitation' = 'sum(Precipitation)')
    dfdailyweather <- rename(dfdailyweather, 'Wind' = 'avg(Wind)')
    dfdailyweather <- rename(dfdailyweather, 'Human_Count' = 'sum(Human_Count)')
    dfdailyweather <- rename(dfdailyweather, 'Dog_Count' = 'sum(Dog_Count)')
    dfdailyweather <- rename(dfdailyweather, 'Car_Count' = 'sum(Car_Count)')
    dfdailyweather <- rename(dfdailyweather, 'Max_Temp' = 'max(Temp)')
    dfdailyweather <- rename(dfdailyweather, 'Min_Temp' = 'min(Temp)')
    dfdailyweather$Max_Temp = as.numeric(dfdailyweather$Max_Temp)
    dfdailyweather$Min_Temp = as.numeric(dfdailyweather$Min_Temp)
    dfdailyweather$Temp_Range <- with(dfdailyweather, ifelse(Temp <= 20, '1- (< 20 Degrees)', ifelse(Temp <= 40, '2- (20-40 Degrees)', ifelse(Temp <= 60, '3- (40-60 Degrees)', ifelse(Temp <= 80, '4- (60-80 Degrees)', ifelse(Temp <= 10000, '5- (> 80 Degrees)'))))))
    dfdailyweather$Precipitation_Range <- with(dfdailyweather, ifelse(Precipitation <= 0.0000001, '1- No Precipitation', ifelse(Precipitation <= 0.25, '2- Light Precipitation', ifelse(Precipitation <= 0.5, '3- Consistent Precipitation', ifelse(Precipitation <= 0.75, '4- Consistent Heavy Precipitation', ifelse(Precipitation <= 80, '5- Storm Precipitation'))))))
    dfdailyweather$Wind_Range <- with(dfdailyweather, ifelse(Wind <= 1, '1- No Wind', ifelse(Wind <= 6, '2- Light Winds', ifelse(Wind <= 15, '3- Moderate Winds', ifelse(Wind <= 30, '4- Strong Winds', ifelse(Wind <= 50, '5- Storm Winds', ifelse(Wind <= 1000, '6- Violent Winds')))))))
    dfdailytemp <- sqldf("select Temp_Range, avg(Temp), avg(Human_Count), avg(Dog_Count), avg(Car_Count), Date from dfdailyweather group by Temp_Range")
    dfdailytemp <- rename(dfdailytemp, 'Temp' = 'avg(Temp)')
    dfdailytemp <- rename(dfdailytemp, 'Human_Count' = 'avg(Human_Count)')
    dfdailytemp <- rename(dfdailytemp, 'Dog_Count' = 'avg(Dog_Count)')
    dfdailytemp <- rename(dfdailytemp, 'Car_Count' = 'avg(Car_Count)')
    dfdailytemp <- rename(dfdailytemp, 'Key' = 'Temp_Range')
    attendancebytempgraph_tall <- dfdailytemp %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
    attendancebytempgraph <- ggplot(attendancebytempgraph_tall, aes(Count, Value, fill = Key))+
      labs(title = 'Attendance by Average Daily Temperature', x = 'Type', y = 'Attendance', color = 'Key:')+
      theme(text = element_text(size = 15))+
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
      theme(text = element_text(size = 15))+
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
      theme(text = element_text(size = 15))+
      geom_col(position = 'dodge')
    output$Plot9 <- renderPlot(attendancebywindgraph)
    
    #Daily Temperature
    dailytempgraph <- ggplot(dfdailyweather, aes(x = Date, group = 1))+
      labs(title = 'Daily Temperature', x = 'Date', y = 'Temperature', color = 'Key:')+
      theme(legend.position = 'top', text = element_text(size = 15))+
      geom_line(aes(y = Temp), color = 'red')+
      geom_point(aes(y = Temp, color = 'Avg Temp'))+
      geom_line(aes(y = Max_Temp), color = 'green')+
      geom_point(aes(y = Max_Temp, color ='Max Temp'))+
      geom_line(aes(y = Min_Temp), color = 'blue')+
      geom_point(aes(y = Min_Temp, color = 'Min Temp'))
    dfdailyweathertempsummary <- c(quantile(dfdailyweather$Temp), mean(dfdailyweather$Temp))
    dailytempdf <- matrix(c( dfdailyweathertempsummary), ncol = 1, nrow = 6)
    rownames(dailytempdf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(dailytempdf) <- c('Temperature')
    output$Table6 <- renderTable(dailytempdf, rownames = T, colnames = T)
    output$Plot11 <- renderPlot(dailytempgraph)
    output$Plot15 <- renderPlot(dailytempgraph)
  
    #Daily Precipitation
    dailyprecipitationgraph <- ggplot(dfdailyweather, aes(x = Date, y = Precipitation))+
      labs(title = 'Daily Precipitation', x = 'Date', y = 'Precipitation (inches)')+
      theme(text = element_text(size = 15))+
      geom_line(color = 'light blue')+
      geom_point()
    dfdailyweatherprecipitationsummary <- c(quantile(dfdailyweather$Precipitation), mean(dfdailyweather$Precipitation))
    dailyprecipitationdf <- matrix(c( dfdailyweatherprecipitationsummary), ncol = 1, nrow = 6)
    rownames(dailyprecipitationdf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(dailyprecipitationdf) <- c('Precipitation')
    output$Table7 <- renderTable(dailyprecipitationdf, rownames = T, colnames = T)
    output$Plot12 <- renderPlot(dailyprecipitationgraph)
    output$Plot16 <- renderPlot(dailyprecipitationgraph)
    #Daily Wind
    dailywindgraph <- ggplot(dfdailyweather, aes(x = Date, y = Wind))+
      labs(title = 'Daily Wind', x = 'Date', y = 'Wind (knots)')+
      theme(text = element_text(size = 15))+
      geom_line(color = 'blueviolet')+
      geom_point()
    dfdailyweatherwindsummary <- c(quantile(dfdailyweather$Wind), mean(dfdailyweather$Wind))
    dailywinddf <- matrix(c(dfdailyweatherwindsummary), ncol = 1, nrow = 6)
    rownames(dailywinddf) <- c('Min', '25th', 'Median', '75th', 'Max', 'AVG')
    colnames(dailywinddf) <- c('Wind')
    output$Table8 <- renderTable(dailywinddf, rownames = T, colnames = T)
    output$Plot13 <- renderPlot(dailywindgraph)
    output$Plot17 <- renderPlot(dailywindgraph)
    
    dfwaste <- read_sheet("https://docs.google.com/spreadsheets/d/1PFyV6ZRtVZwAeL2Yd7U612iIdHUEx4QQ1MUM_oBoM48/edit#gid=0")
    dfwaste <- sqldf("select * from df2
             LEFT JOIN dfwaste
             ON df2.Date1 = dfwaste.Date1")
    #dfwaste <- dfwaste %>% distinct()
    dup <- duplicated(colnames(dfwaste))
    dfwaste <- dfwaste[!dup]
    dfwastegraph <- ggplot(dfwaste, aes(x = Date1, group = 1))+
      labs(title = 'Monthly Waste Bags', x = 'Month - Year', y = '# of Bags Replaced')+
      theme(text = element_text(size = 15))+
      geom_line(aes(y = Count), color = 'orange')+
      geom_point(aes(y = Count))
    output$Plot10 <- renderPlot(dfwastegraph)

  })
  
  
  observeEvent(input$Click2, {
    dfpoop <- read_sheet("https://docs.google.com/spreadsheets/d/1PFyV6ZRtVZwAeL2Yd7U612iIdHUEx4QQ1MUM_oBoM48/edit#gid=0")
    Year <- input$Text9
    Month <- input$Text8
    Count_Box <- input$Text7
    Count_Box <- as.numeric(Count_Box)
    Count <- Count_Box * 200
    newdf <- data.frame(Year, Month, Count)
    newdf$Year <- str_glue("- {newdf$Year}")
    newdf$Date1 <- paste(newdf$Month, newdf$Year)
    newdf <- sqldf("select * from dfpoop
                       union
                       select * from newdf")
    write_sheet(newdf, "1PFyV6ZRtVZwAeL2Yd7U612iIdHUEx4QQ1MUM_oBoM48", sheet = 1)
    dfpoop <- read_sheet("https://docs.google.com/spreadsheets/d/1PFyV6ZRtVZwAeL2Yd7U612iIdHUEx4QQ1MUM_oBoM48/edit#gid=0")
    dfpoopgraph <- ggplot(dfpoop, aes(x = Date1, group = 1))+
      labs(title = 'Monthly Waste Bags', x = 'Month - Year', y = '# of Bags Replaced')+
      geom_line(aes(y = Count), color = 'orange')+
      geom_point(aes(y = Count))
    output$Plot10 <- renderPlot(dfpoopgraph)
    
  })
  
  
})

shinyApp(ui, server)
