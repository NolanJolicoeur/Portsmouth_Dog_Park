#Need to pay attention to yearly data.  Some graphs can become too conjested after too much time. 
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
library(knitr)
library(kableExtra)
#Create and view data frame
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
df1$Day_Type <- factor((weekdays(df1$Date) %in% weekdays1), 
                    levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
df1$Time <- as.numeric(df1$Time)
df1$Time_of_Day <- with(df1, ifelse(Time <= 3, '1- (12AM - 2AM)', ifelse(Time <= 6, '2- (3AM - 5AM)', ifelse(Time <= 9, '3- (6AM - 8AM)', ifelse(Time <= 12, '4- (9AM - 11AM)', ifelse(Time <= 15, '5- (12PM - 2PM)', ifelse(Time <= 18, '6- (3PM - 5PM)', ifelse(Time <= 21, '7- (6PM - 8PM)', ifelse(Time <= 24, '8- (9PM - 11PM)')))))))))
df1$Time_Official <- paste(df1$Date, df1$Time)  
df1 <- sqldf("select Year, Month, Day, Time, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count), Date, Day_Name, Day_Type, Time_of_Day, Time_Official from df1 group by Time_Official")
df1 <- rename(df1, 'TrafX_Count' = 'sum(TrafX_Count)')
df1 <- rename(df1, 'Human_Count' = 'sum(Human_Count)')
df1 <- rename(df1, 'Dog_Count' = 'sum(Dog_Count)')
df1 <- rename(df1, 'Car_Count' = 'sum(Car_Count)')
df1$Time_Official <- NULL
df1$Day_Type_Time_of_Day <- paste(df1$Day_Type, df1$Time_of_Day)
df1 <- df1[order(df1$Date, df1$Time),]
df1<-df1 %>% mutate(Week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% arrange(Date)
View(df1)

#Group data by time of day 
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
dfavgtimeofday_graph

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
dfavgDay_Type_graph

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
dfavgDay_Type_Time_of_Day_Weekend_Tall_graph
#Graph for Weekdays 
dfavgDay_Type_Time_of_Day_Weekday <-sqldf("select Time_of_Day, Time, Day_Type, TrafX_Count, Human_Count, Dog_Count, Car_Count from dfavgDay_Type_Time_of_Day where Day_Type == 'Weekday'")
dfavgDay_Type_Time_of_Day_Weekday <- rename(dfavgDay_Type_Time_of_Day_Weekday, 'Key' = 'Time_of_Day')
dfavgDay_Type_Time_of_Day_Weekday_Tall <- dfavgDay_Type_Time_of_Day_Weekday %>% gather(key = Count, value = Value, c(Human_Count, Dog_Count, Car_Count))
dfavgDay_Type_Time_of_Day_Weekday_Tall_graph <- ggplot(dfavgDay_Type_Time_of_Day_Weekday_Tall, aes(Count, Value, fill = Key))+
  labs(title = 'Average Attendance on Weekdays Per Time of Day', x = 'Time', y = 'Count', color = 'Key:')+
  geom_col(position = 'dodge')+
  lims(y = c(0,100))
dfavgDay_Type_Time_of_Day_Weekday_Tall_graph
grid.arrange(dfavgDay_Type_Time_of_Day_Weekend_Tall_graph, dfavgDay_Type_Time_of_Day_Weekday_Tall_graph, ncol=2)
# Tables for each of the above graphs 
weekdaysdogssummary <- quantile(dfavgDay_Type_Time_of_Day_Weekday$Dog_Count)
weekdayshumanssummary <- quantile(dfavgDay_Type_Time_of_Day_Weekday$Human_Count)
weekdayscarssummary <- quantile(dfavgDay_Type_Time_of_Day_Weekday$Car_Count)
weekdaystattable <- matrix(c(weekdaysdogssummary, weekdayshumanssummary, weekdayscarssummary), nrow = 5)
colnames(weekdaystattable) <- c('Dogs', 'Humans', 'Cars')
rownames(weekdaystattable) <- c('Min','25th','Median','75th', 'Max')
weekdaystattable <- as.data.frame(weekdaystattable)

weekendsdogssummary <- quantile(dfavgDay_Type_Time_of_Day_Weekend$Dog_Count)
weekendshumanssummary <- quantile(dfavgDay_Type_Time_of_Day_Weekend$Human_Count)
weekendscarssummary <- quantile(dfavgDay_Type_Time_of_Day_Weekend$Car_Count)
weekendstattable <- matrix(c(weekendsdogssummary, weekendshumanssummary, weekendscarssummary), nrow = 5)
colnames(weekendstattable) <- c('Dogs', 'Humans', 'Cars')
rownames(weekendstattable) <- c('Min','25th','Median','75th', 'Max')
weekendstattable <- as.data.frame(weekendstattable)

weekdaystattable <- kable(weekdaystattable) %>%
  kable_styling(full_width = FALSE, position = "float_left")
weekendstattable <- kable(weekendstattable) %>%
  kable_styling(full_width = FALSE, position = "left")
print(weekdaystattable)
print(weekendstattable)

#Attendance for the week 
attendance_by_week <- sqldf("select Date, Week, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df1 group by Week")
attendance_by_week <- rename(attendance_by_week, 'TrafX_Count' = 'sum(TrafX_Count)')
attendance_by_week <- rename(attendance_by_week, 'Human_Count' = 'sum(Human_Count)')
attendance_by_week <- rename(attendance_by_week, 'Dog_Count' = 'sum(Dog_Count)')
attendance_by_week <- rename(attendance_by_week, 'Car_Count' = 'sum(Car_Count)')
#Graph
attendance_by_week_graph <- ggplot(attendance_by_week, aes(x = Date, group = 1))+
  labs(title = 'Total Attendance by the Week', x = 'Week', y = 'Total Attendance', color = 'Key:')+
  geom_line(aes(y = Dog_Count), color = 'green')+
  geom_point(aes(y = Dog_Count, color = 'Dogs'))+
  geom_line(aes(y = Human_Count), color = 'blue')+
  geom_point(aes(y = Human_Count, color = 'Humans'))+ 
  geom_line(aes(y = Car_Count), color = 'red')+
  geom_point(aes(y = Car_Count, color = 'Cars'))
attendance_by_week_graph

#Attendance by Month
attendance_by_month <- sqldf("select Date, Month, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df1 group by Month")
attendance_by_month <- rename(attendance_by_month, 'TrafX_Count' = 'sum(TrafX_Count)')
attendance_by_month <- rename(attendance_by_month, 'Human_Count' = 'sum(Human_Count)')
attendance_by_month <- rename(attendance_by_month, 'Dog_Count' = 'sum(Dog_Count)')
attendance_by_month <- rename(attendance_by_month, 'Car_Count' = 'sum(Car_Count)')
#Graph 
attendance_by_month_graph <- ggplot(attendance_by_month, aes(Date, group = 1))+
  labs(title = 'Total Attendance by the Month', x = 'Month', y = 'Total Attendance', color = 'Key:')+
  geom_line(aes(y = Dog_Count), color = 'green')+
  geom_point(aes(y = Dog_Count, color = 'Dogs'))+
  geom_line(aes(y = Human_Count), color = 'blue')+
  geom_point(aes(y = Human_Count, color = 'Humans'))+ 
  geom_line(aes(y = Car_Count), color = 'red')+
  geom_point(aes(y = Car_Count, color = 'Cars'))
attendance_by_month_graph







