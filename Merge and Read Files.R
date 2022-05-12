library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)

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
saveRDS(df1, 'Data_Log.R')
saveRDS(files, 'File_Log.R')
