
update <- function(data_table, files){
  log <- readRDS('File_Log.R')
  for (i in files){
    if (i %in% log){
      otherdf <- read.delim(i)
      otherdf <- as.data.frame(otherdf)
      otherdf <- otherdf[grep('00000',otherdf$D),]
      otherdf <- as.data.frame(otherdf)
      otherdf <- otherdf %>% separate(otherdf, c('Year','Month','Day', 'Time','Blank', 'TrafX_Count','Blank2')) 
      otherdf$TrafX_Count <- as.numeric(otherdf$TrafX_Count)
      otherdf <- sqldf("select Year, Month, Day, Time, TrafX_Count from otherdf")
      df1 = sqldf("
               SELECT * FROM data_table
               UNION
               SELECT * FROM otherdf
               ")
    }
  }
  log <- saveRDS(files, 'File_Log.R')
}
files <- dir('Shuttle_Files')
update(df1, files)
