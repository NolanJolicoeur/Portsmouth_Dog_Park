library(sqldf)
library(tidyr)
files = grep(".TXT", dir('Shuttle_Files'), value = T)
df <- data.frame(Year = character(),
                 Month = character(),
                 Day = character(), 
                 Time = character(), 
                 TrafX_Count = integer())
stopifnot(class(df) == 'data.frame')
stopifnot(length(colnames(df)) == 5)
for (t in dir('Shuttle_Files')){
  otherdf <- read.delim(t)
  otherdf <- as.data.frame(otherdf)
  otherdf <- otherdf[grep('00000',otherdf$D),]
  otherdf <- as.data.frame(otherdf)
  otherdf <- otherdf %>% separate(otherdf, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
  otherdf$TrafX_Count <- as.numeric(otherdf$TrafX_Count)
  otherdf <- sqldf("select Year, Month, Day, Time, TrafX_Count from otherdf")
  df = sqldf("
               SELECT * FROM df
               UNION
               SELECT * FROM otherdf
               ")
}
