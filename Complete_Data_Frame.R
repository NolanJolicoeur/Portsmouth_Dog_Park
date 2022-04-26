#Load libraries
library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)

#Create and view data frame for shuttle files
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
  df3 = sqldf("
               SELECT * FROM df
               UNION
               SELECT * FROM otherdf
               ")
} 
setwd('../')
df3$Human_Count <- round((0.3673 * df3$TrafX_Count),0)
df3$Dog_Count <- round((0.4591 * df3$TrafX_Count),0)
df3$Car_Count <- round((0.2959 * df3$TrafX_Count),0)
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
day1 <- 12
year2 <- 2022
month2 <- 3 
day2 <- 30
url = glue::glue("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=UUU&data=all&year1={year1}&month1={month1}&day1={day1}&year2={year2}&month2={month2}&day2={day2}&tz=Etc%2FUTC&format=onlycomma&latlon=no&elev=no&missing=M&trace=T&direct=no&report_type=1&report_type=2")
df2 <- read.csv(url)
df2 <- df2[df2$tmpf != 'M',]
df2$valid <- gsub("-"," ",df2$valid, fixed = TRUE)
df2$valid <- gsub(":"," ",df2$valid, fixed = TRUE)
df2 <- df2 %>% separate(valid, c('Year','Month','Day', 'Time','Minute'),' ') 
df2$Day_Time <- paste(df2$Day, df2$Time)
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
df2 <- df2[,c('Temp', 'Precipitation','Wind', 'Date_Time')]

#Join data frames
df1 <- sqldf("select * from df2
             LEFT JOIN df3
             ON df2.Date_Time = df3.Date_Time")
df1 <- df1 %>% drop_na()





