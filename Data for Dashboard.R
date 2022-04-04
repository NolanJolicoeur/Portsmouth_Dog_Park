#Load Packages
library(stringr)
library(tidyr)
library(dplyr)
library(sqldf)
library(lubridate)
library(ggplot2)
library(data.table)

#Get base dataframe
df1 <- read.delim(file.choose())
df1 <- as.data.frame(df1)
df1 <- df1[grep('00000',df1$D),]
df1 <- as.data.frame(df1)
df1 <- df1 %>% separate(df1, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
#b <- as.data.frame(b)
#b <- df1[grep('00000',b$D),]
#b <- as.data.frame(b)
#b <- df1 %>% separate(b, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
#df1 <- sqldf("select * from a
             #union all 
             #select * from b")
df1$TrafX_Count <- as.numeric(df1$TrafX_Count)
df1 <- sqldf("select Year, Month, Day, Time, TrafX_Count from df1")
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
View(df1)

#Hourly data with graphs
#Group data by time
dfavghourly <- sqldf("select Time, avg(TrafX_Count), avg(Human_Count), avg(Dog_Count), avg(Car_Count) from df1 group by Time")
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

#Daily data with graphs 
#Group data by date
dfdaily <- sqldf('select Date, Day_Name, W_Day, sum(TrafX_Count), sum(Human_Count), sum(Dog_Count), sum(Car_Count) from df1 group by Date')
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



