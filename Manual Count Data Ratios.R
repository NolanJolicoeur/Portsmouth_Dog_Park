df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQeQ0YiEBVASJN0EukDk3Y6CB8rf9aaQJ-d0ViW_nWYf_3gsHHcYu87eKQIgzFOpM_mV9stuA75x0zw/pub?gid=0&single=true&output=csv")
df$Day_Name <- weekdays(df$Date, abbreviate = FALSE)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df$Day_Type <- factor((weekdays(df$Date, abbreviate = FALSE) %in% weekdays1), 
                    levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
df$People_Ratio <- df$People/df$TrafX_Count
df$Dog_Ratio <- df$Dogs/df$TrafX_Count
df$Cars_Ratio <- df$Cars/df$TrafX_Count
df$Error <- (df$Actual_Total-df$TrafX_Count)/df$TrafX_Count
mean(df$Error)
#This code chunk is giving the general ratio of people, dogs, and cars to Traf_X count 
people_to_count_ratio <- (sum(df$People))/(sum(df$TrafX_Count))
dogs_to_count_ratio <- (sum(df$Dogs))/(sum(df$TrafX_Count))
cars_to_count_ratio <- (sum(df$Cars))/(sum(df$TrafX_Count))
ratio_table <- matrix(c(people_to_count_ratio, dogs_to_count_ratio, cars_to_count_ratio), nrow = 3)
colnames(ratio_table) <- c('Ratio')
rownames(ratio_table) <- c('People', 'Dogs', 'Cars')
ratio_table <- as.table(ratio_table)
ratio_table
