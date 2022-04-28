library(sqldf)
library(tidyr)
process <- function(data){
  df <- as.data.frame(data)
  df <- df[grep('00000',df$D),]
  df <- as.data.frame(df)
  df <- df %>% separate(df, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
  df$TrafX_Count <- as.numeric(df$TrafX_Count)
  df <- sqldf("select Year, Month, Day, Time, TrafX_Count from df")
  return(df)
}
d <- read.delim('~/Dog_Park/Shuttle_Files/ShuttleFile1.TXT')
a <- process(d)
