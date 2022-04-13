#February and March people in park total, busiest day of the week, max values
#Noaa for weather data or Tony Patraca
#Load Packages
library(stringr)
library(tidyr)
library(dplyr)
library(sqldf)
library(lubridate)
library(ggplot2)
library(data.table)
library(tidyverse)
library(forcats)
library(gridExtra)

df <- data.frame(Year = character(),
                 Month = character(),
                 Day = character(), 
                 Time = character(), 
                 TrafX_Count = integer())
stopifnot(class(df) == 'data.frame')
stopifnot(length(colnames(df)) == 5)
files <- dir('Shuttle_Files')
setwd('Shuttle_Files')
for (t in files){
  otherdf <- read.delim(t)
  otherdf <- as.data.frame(otherdf)
  otherdf <- otherdf[grep('00000',otherdf$D),]
  otherdf <- as.data.frame(otherdf)
  otherdf <- otherdf %>% separate(otherdf, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
  otherdf$TrafX_Count <- as.numeric(otherdf$TrafX_Count)
  otherdf <- sqldf("select Year, Month, Day, Time, TrafX_Count from otherdf")
  df1 = sqldf("
               SELECT * FROM df
               UNION
               SELECT * FROM otherdf
               ")
} 
setwd('../')
df1$Human_Count <- round((0.3673 * df1$TrafX_Count),0)
df1$Dog_Count <- round((0.4591 * df1$TrafX_Count),0)
df1$Car_Count <- round((0.2959 * df1$TrafX_Count),0)
df1$Year <- str_glue("20{df1$Year}")
df1$Day <- str_glue("-{df1$Day}")
df1$Month <- str_glue("-{df1$Month}")
df1$Date <- paste( df1$Year, df1$Month, df1$Day)
df1<-df1 %>% mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
df1$Date <- as.Date(df1$Date)
df1$Day_Name <- weekdays(df1$Date)
df1<-df1 %>% mutate(across(where(is.character), str_remove_all, pattern = fixed("-")))
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df1$W_Day <- factor((weekdays(df1$Date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
df1$Time <- as.numeric(df1$Time)
df1$Time_of_Day <- with(df1, ifelse(Time <= 3, '1- (12AM - 2AM)', ifelse(Time <= 6, '2- (3AM - 5AM)', ifelse(Time <= 9, '3- (6AM - 8AM)', ifelse(Time <= 12, '4- (9AM - 11AM)', ifelse(Time <= 15, '5- (12PM - 2PM)', ifelse(Time <= 18, '6- (3PM - 5PM)', ifelse(Time <= 21, '7- (6PM - 8PM)', ifelse(Time <= 24, '8- (9PM - 11PM)')))))))))
View(df1)

df1$Time_Official <- paste(df1$Date, df1$Time)  
Official_df <- sqldf("select Year, Month, Day, Time, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count), Date, Day_Name, W_Day, Time_of_Day, Time_Official from df1 group by Time_Official")
Official_df <- rename(Official_df, 'TrafX_Count' = 'sum(TrafX_Count)')
Official_df <- rename(Official_df, 'Human_Count' = 'sum(Human_Count)')
Official_df <- rename(Official_df, 'Dog_Count' = 'sum(Dog_Count)')
Official_df <- rename(Official_df, 'Car_Count' = 'sum(Car_Count)')
#Hourly data with graphs
#Group data by time
dfavghourly <- sqldf("select Time, Time_of_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from Official_df group by Time")
dfavghourly <- rename(dfavghourly, 'TrafX_Count' = 'avg(TrafX_Count)')
dfavghourly <- rename(dfavghourly, 'Human_Count' = 'avg(Human_Count)')
dfavghourly <- rename(dfavghourly, 'Dog_Count' = 'avg(Dog_Count)')
dfavghourly <- rename(dfavghourly, 'Car_Count' = 'avg(Car_Count)')
#Graph 
hourly_avg_graph <- ggplot(dfavghourly, aes(x = Time, group = 1))+
  labs(title = 'Average Dog, Human, and Car Count by the Hour', x = 'Time', y = 'Count', color = 'Key:')+
  geom_line(aes(y = Dog_Count), color = 'green')+
  geom_point(aes(y = Dog_Count, color = 'Dogs'))+
  geom_line(aes(y = Human_Count), color = 'blue')+
  geom_point(aes(y = Human_Count, color = 'Humans'))+
  geom_line(aes(y = Car_Count), color = 'red')+
  geom_point(aes(y = Car_Count, color = 'Cars'))+
  scale_y_continuous(breaks = scales::breaks_width(2))
hourly_avg_graph
#Find Max and Min Values and put in tables
dogsavghourly <- c(max(dfavghourly$Dog_Count), min(dfavghourly$Dog_Count))
humansavghourly <- c(max(dfavghourly$Human_Count), min(dfavghourly$Human_Count))
carsavghourly <- c(max(dfavghourly$Car_Count), min(dfavghourly$Car_Count))
hourlyavgstattable <- matrix(c(dogsavghourly, humansavghourly, carsavghourly), nrow = 2)
colnames(hourlyavgstattable) <- c('Dogs', 'Humans', 'Cars')
rownames(hourlyavgstattable) <- c('Max', 'Min')
hourlyavgstattable <- as.table(hourlyavgstattable)
hourlyavgstattable

#Group data by time of day 
dfavgtimeofday1 <- sqldf("select Time, Time_of_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from Official_df group by Time")
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
dfavgtimeofday_graph

#Daily data with graphs 
#Group data by date
dfdaily <- sqldf('select Date, Day_Name, W_Day, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from Official_df group by Date')
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
daily_graph
#Find Summary Stats for daily data and build table 
dailydogssummary <- quantile(dfdaily$Dog_Count)
dailyhumanssummary <- quantile(dfdaily$Human_Count)
dailycarssummary <- quantile(dfdaily$Car_Count)
dailystattable <- matrix(c(dailydogssummary, dailyhumanssummary, dailycarssummary), nrow = 5)
colnames(dailystattable) <- c('Dogs', 'Humans', 'Cars')
rownames(dailystattable) <- c('Min','25th','Median','75th', 'Max')
dailystattable <- as.table(dailystattable)
dailystattable

#Group data by day name
dfavgdaily <- sqldf('select Day_Name, W_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfdaily group by Day_Name')
dfavgdaily <- rename(dfavgdaily, 'TrafX_Count' = 'avg(TrafX_Count)')
dfavgdaily <- rename(dfavgdaily, 'Human_Count' = 'avg(Human_Count)')
dfavgdaily <- rename(dfavgdaily, 'Dog_Count' = 'avg(Dog_Count)')
dfavgdaily <- rename(dfavgdaily, 'Car_Count' = 'avg(Car_Count)')
#Graph
daily_avg_graph <- ggplot(dfavgdaily, aes(x = Day_Name, group = 1))+
  labs(title = 'Average Dog, Human, and Car Count by the Day of the Week', x = 'Day of Week', y = 'Count', color = 'Key:')+
  geom_line(aes(y = Dog_Count), color = 'green')+
  geom_point(aes(y = Dog_Count, color = 'Dogs'))+
  geom_line(aes(y = Human_Count), color = 'blue')+
  geom_point(aes(y = Human_Count, color = 'Humans'))+
  geom_line(aes(y = Car_Count), color = 'red')+
  geom_point(aes(y = Car_Count, color = 'Cars'))+
  lims(y = c(0,200))
daily_avg_graph
#Find Max and Min Values and put in tables
dogsavgdaily <- c(max(dfavgdaily$Dog_Count), min(dfavgdaily$Dog_Count))
humansavgdaily <- c(max(dfavgdaily$Human_Count), min(dfavgdaily$Human_Count))
carsavgdaily <- c(max(dfavgdaily$Car_Count), min(dfavgdaily$Car_Count))
dailyavgstattable <- matrix(c(dogsavgdaily, humansavgdaily, carsavgdaily), nrow = 2)
colnames(dailyavgstattable) <- c('Dogs', 'Humans', 'Cars')
rownames(dailyavgstattable) <- c('Max', 'Min')
dailyavgstattable <- as.table(dailyavgstattable)
dailyavgstattable

#Group by weekend and weekday
dfavgW_Day <- sqldf("select  W_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfavgdaily group by W_Day")
dfavgW_Day <- rename(dfavgW_Day, 'TrafX_Count' = 'avg(TrafX_Count)')
dfavgW_Day <- rename(dfavgW_Day, 'Human_Count' = 'avg(Human_Count)')
dfavgW_Day <- rename(dfavgW_Day, 'Dog_Count' = 'avg(Dog_Count)')
dfavgW_Day <- rename(dfavgW_Day, 'Car_Count' = 'avg(Car_Count)')
dfavgW_Day <- rename(dfavgW_Day, 'Key' = 'W_Day')
#Graph
dfavgW_Day_Tall <- dfavgW_Day %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
dfavgW_Day_graph <- ggplot(dfavgW_Day_Tall, aes(Count, Value, fill = Key))+
  labs(title = 'Average Attendance on Weekends vs Weekdays', x = 'Type', y = 'Count', color = 'Key:')+
  geom_col(position = 'dodge')
dfavgW_Day_graph

#Group by Weekday or Weekend and Time of Day
df2 <- df1
df2$Date_Time_of_Day <- paste(df2$Date, df2$Time_of_Day)
df2$W_Day_Time_of_Day <- paste(df2$W_Day, df2$Time_of_Day)
dfavgW_Day_Time_of_Day1 <- sqldf("select W_Day_Time_of_Day, Time_of_Day, Date_Time_of_Day, Date, Time, W_Day, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df2 group by Date_Time_of_Day")
dfavgW_Day_Time_of_Day1 <- rename(dfavgW_Day_Time_of_Day1, 'TrafX_Count' = 'sum(TrafX_Count)')
dfavgW_Day_Time_of_Day1 <- rename(dfavgW_Day_Time_of_Day1, 'Human_Count' = 'sum(Human_Count)')
dfavgW_Day_Time_of_Day1 <- rename(dfavgW_Day_Time_of_Day1, 'Dog_Count' = 'sum(Dog_Count)')
dfavgW_Day_Time_of_Day1 <- rename(dfavgW_Day_Time_of_Day1, 'Car_Count' = 'sum(Car_Count)')
dfavgW_Day_Time_of_Day <- sqldf("select Time_of_Day, Time, W_Day, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from dfavgW_Day_Time_of_Day1 group by W_Day_Time_of_Day")
dfavgW_Day_Time_of_Day <- rename(dfavgW_Day_Time_of_Day, 'TrafX_Count' = 'avg(TrafX_Count)')
dfavgW_Day_Time_of_Day <- rename(dfavgW_Day_Time_of_Day, 'Human_Count' = 'avg(Human_Count)')
dfavgW_Day_Time_of_Day <- rename(dfavgW_Day_Time_of_Day, 'Dog_Count' = 'avg(Dog_Count)')
dfavgW_Day_Time_of_Day <- rename(dfavgW_Day_Time_of_Day, 'Car_Count' = 'avg(Car_Count)')
#Graph for Weekends
dfavgW_Day_Time_of_Day_Weekend <-sqldf("select Time_of_Day, Time, W_Day, TrafX_Count, Human_Count, Dog_Count, Car_Count from dfavgW_Day_Time_of_Day where W_Day == 'Weekend'")
dfavgW_Day_Time_of_Day_Weekend <- rename(dfavgW_Day_Time_of_Day_Weekend, 'Key' = 'Time_of_Day')
dfavgW_Day_Time_of_Day_Weekend_Tall <- dfavgW_Day_Time_of_Day_Weekend %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
dfavgW_Day_Time_of_Day_Weekend_Tall_graph <- ggplot(dfavgW_Day_Time_of_Day_Weekend_Tall, aes(Count, Value, fill = Key))+
  labs(title = 'Average Attendance on Weekends Per Time of Day', x = 'Time', y = 'Count', color = 'Key:')+
  geom_col(position = 'dodge')+
  lims(y = c(0,100))
dfavgW_Day_Time_of_Day_Weekend_Tall_graph
#Graph for Weekdays 
dfavgW_Day_Time_of_Day_Weekday <-sqldf("select Time_of_Day, Time, W_Day, TrafX_Count, Human_Count, Dog_Count, Car_Count from dfavgW_Day_Time_of_Day where W_Day == 'Weekday'")
dfavgW_Day_Time_of_Day_Weekday <- rename(dfavgW_Day_Time_of_Day_Weekday, 'Key' = 'Time_of_Day')
dfavgW_Day_Time_of_Day_Weekday_Tall <- dfavgW_Day_Time_of_Day_Weekday %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
dfavgW_Day_Time_of_Day_Weekday_Tall_graph <- ggplot(dfavgW_Day_Time_of_Day_Weekday_Tall, aes(Count, Value, fill = Key))+
  labs(title = 'Average Attendance on Weekdays Per Time of Day', x = 'Time', y = 'Count', color = 'Key:')+
  geom_col(position = 'dodge')+
  lims(y = c(0,100))
dfavgW_Day_Time_of_Day_Weekday_Tall_graph
grid.arrange(dfavgW_Day_Time_of_Day_Weekend_Tall_graph, dfavgW_Day_Time_of_Day_Weekday_Tall_graph, ncol=2)




