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


ui <- shinyUI(fluidPage(
    #Main Page Title and tab to upload files for dog park data and weather data. 
    titlePanel("Dog Park Dashboard"),
    tabsetPanel(
        #Tab Panel for Daily dog park data breakdown.  Includes y and x-axis selector and graph. 
        tabPanel("Daily Summary",
                 mainPanel(
                     plotOutput('Plot1'),
                     tableOutput('Table1'),
                     plotOutput('Plot2'),
                     tableOutput('Table2'),
                     plotOutput('Plot3'),
                     tableOutput('Table3'),
                     textOutput('Text')
                 )),
        tabPanel('Weekend vs Weekday',
                 mainPanel(
                     plotOutput('Plot4'),
                     tableOutput('Table4'),
                     plotOutput('Plot5')
                 )),
        tabPanel('Monthly/Seasonal Summary',
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
    #Data input
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
    
    
    #Create and view data frame for shuttle files
    
    process <- function(data1){
        df <- as.data.frame(data1)
        df <- df[grep('00000',df$D),]
        df <- as.data.frame(df)
        df <- df %>% separate(df, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
        df <- sqldf("select Year, Month, Day, Time, TrafX_Count from df")
        df$TrafX_Count <- as.numeric(df$TrafX_Count)
        df$Date_Time <- paste( df$Year, df$Month, df$Day, df$Time)
        df <- sqldf("select Year, Month, Day, Time, sum(TrafX_Count), Date_Time from df group by Date_Time")
        df <- rename(df, 'TrafX_Count' = 'sum(TrafX_Count)')
        return(df)
    }
    
    
    df3 <- data.frame(Year = character(),
                      Month = character(),
                      Day = character(), 
                      Time = character(), 
                      TrafX_Count = integer(),
                      Date_Time = numeric())
    stopifnot(class(df3) == 'data.frame')
    stopifnot(length(colnames(df3)) == 6)
    files <- dir('Shuttle_Files')
    setwd('Shuttle_Files')
    for (t in files){
        otherdf <- read.delim(t)
        otherdf <- process(otherdf)
        df3 = sqldf("
               SELECT * FROM df3
               UNION
               SELECT * FROM otherdf
               ")
    } 
    setwd('../')
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
    
    #Create data frame for weather data 
    year1 <- 2022
    month1 <- 2
    day1 <- 10
    year2 <- 2022
    month2 <- 4 
    day2 <- 30
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
    df2$Time <- as.numeric(df2$Time)
    df2$Date_Time <- paste(df2$Date, df2$Time)
    
    #Join data frames
    df4 <- sqldf("select * from df2
             LEFT JOIN df3
             ON df2.Date_Time = df3.Date_Time")
    df4 <- df4 %>% drop_na()
    duplicates <- duplicated(colnames(df4))
    df4 <- df4[!duplicates]
    
    #File upload
    files <- readRDS('File_Log.R')
    output$contents <- renderText(files)
    
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
        geom_col(position = 'dodge')
    output$Plot1 <- renderPlot(dfavgtimeofday_graph)
    #table
    fivenumbersummarytimeofdaydf <- matrix(c( quantile(dfavgtimeofday$Car_Count), quantile(dfavgtimeofday$Dog_Count), quantile(dfavgtimeofday$Human_Count)), ncol = 3, nrow = 5)
    rownames(fivenumbersummarytimeofdaydf) <- c('Min', '25th', 'Median', '75th', 'Max')
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
        geom_line(aes(y = Dog_Count), color = 'green')+
        geom_point(aes(y = Dog_Count, color = 'Dogs'))+
        geom_line(aes(y = Human_Count), color = 'blue')+
        geom_point(aes(y = Human_Count, color = 'Humans'))+ 
        geom_line(aes(y = Car_Count), color = 'red')+
        geom_point(aes(y = Car_Count, color = 'Cars'))+
        scale_y_continuous(breaks = scales::breaks_width(50))
    output$Plot2 <- renderPlot(daily_graph)
    #Table 
    fivenumbersummarydailydf <- matrix(c( quantile(dfdaily$Car_Count), quantile(dfdaily$Dog_Count), quantile(dfdaily$Human_Count)), ncol = 3, nrow = 5)
    rownames(fivenumbersummarydailydf) <- c('Min', '25th', 'Median', '75th', 'Max')
    colnames(fivenumbersummarydailydf) <- c('Cars', 'Dogs', 'Humans')
    output$Table2 <- renderTable(fivenumbersummarydailydf, rownames = T, colnames = T)
    
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
    #Table 
    fivenumbersummarydailyavgdf <- matrix(c( quantile(dfavgdaily$Car_Count), quantile(dfavgdaily$Dog_Count), quantile(dfavgdaily$Human_Count)), ncol = 3, nrow = 5)
    rownames(fivenumbersummarydailyavgdf) <- c('Min', '25th', 'Median', '75th', 'Max')
    colnames(fivenumbersummarydailyavgdf) <- c('Cars', 'Dogs', 'Humans')
    output$Table3 <- renderTable(fivenumbersummarydailyavgdf, rownames = T, colnames = T)
    
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
    #Table 
    fivenumbersummarydaytypedf <- matrix(c( quantile(Weekenddf$Car_Count), quantile(Weekenddf$Dog_Count), quantile(Weekenddf$Human_Count), quantile(Weekdaydf$Car_Count), quantile(Weekdaydf$Dog_Count), quantile(Weekdaydf$Human_Count)), ncol = 6, nrow = 5)
    rownames(fivenumbersummarydaytypedf) <- c('Min', '25th', 'Median', '75th', 'Max')
    colnames(fivenumbersummarydaytypedf) <- c('Cars (Weekend)', 'Dogs (Weekend)', 'Humans (Weekend)', 'Cars (Weekdays)', 'Dogs (Weekdays)', 'Humans (Weekdays)')
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
    attendance_by_month <- sqldf("select Date, Month, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df4 group by Month")
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
    dfdailyweather <- sqldf("select Date, avg(Temp), sum(Precipitation), avg(Wind), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df4 group by Date")
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

