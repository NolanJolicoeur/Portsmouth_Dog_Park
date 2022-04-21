library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)
year1 <- 2022
month1 <- 4
day1 <- 3
year2 <- 2022
month2 <- 4 
day2 <- 5
url = glue::glue("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=UUU&data=all&year1={year1}&month1={month1}&day1={day1}&year2={year2}&month2={month2}&day2={day2}&tz=Etc%2FUTC&format=onlycomma&latlon=no&elev=no&missing=M&trace=T&direct=no&report_type=1&report_type=2")
df <- read.csv(url)
df <- df[df$tmpf != 'M',]
df$valid <- gsub("-"," ",df$valid, fixed = TRUE)
df$valid <- gsub(":"," ",df$valid, fixed = TRUE)
df <- df %>% separate(valid, c('Year','Month','Day', 'Time','Minute'),' ') 
df$Day_Time <- paste(df$Day, df$Time)
df <- sqldf("select * from df group by Day_Time")
df$Day <- str_glue("-{df$Day}")
df$Month <- str_glue("-{df$Month}")
df$Date <- paste( df$Year, df$Month, df$Day)
df<-df %>% mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
df$Date <- as.Date(df$Date)
df<-df %>% mutate(across(where(is.character), str_remove_all, pattern = fixed("-")))
df <- df[c('Date', 'Year', 'Month', 'Day', 'Time', 'tmpf', 'p01i', 'gust')]
df <- rename(df, 'Temp' = 'tmpf')
df <- rename(df, 'Precipitation' = 'p01i')
df <- rename(df, 'Wind' = 'gust')
View(df)




