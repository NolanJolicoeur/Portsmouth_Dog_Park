library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)

df <- readRDS('Data_Log.R')
log <- readRDS('File_Log.R')
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


#Test Case
df <- readRDS('Data_Log.R')
log <- readRDS('File_Log.R')
files <- dir('Shuttle_Files')
files <- files[1]
df <- df %>% top_n(10)
View(df)
#Notice that there are only 10 columns in df at this point
New_Files <- setdiff(log,files)
setwd('Shuttle_Files')
for (i in New_Files){
  otherdf <- read.delim(i)
  otherdf <- process(otherdf)
  df = sqldf("
               SELECT * FROM df
               UNION
               SELECT * FROM otherdf
               ")
} 
setwd('../')
#The new data is added