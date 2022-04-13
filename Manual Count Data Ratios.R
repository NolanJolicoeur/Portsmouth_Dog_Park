df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQeQ0YiEBVASJN0EukDk3Y6CB8rf9aaQJ-d0ViW_nWYf_3gsHHcYu87eKQIgzFOpM_mV9stuA75x0zw/pub?gid=0&single=true&output=csv")
people_to_count_ratio <- (sum(df$People))/(sum(df$Actual_Total))
dogs_to_count_ratio <- (sum(df$Dogs))/(sum(df$Actual_Total))
cars_to_count_ratio <- (sum(df$Cars))/(sum(df$Actual_Total))
ratio_table <- matrix(c(people_to_count_ratio, dogs_to_count_ratio, cars_to_count_ratio), nrow = 3)
colnames(ratio_table) <- c('Ratio')
rownames(ratio_table) <- c('People', 'Dogs', 'Cars')
ratio_table <- as.table(ratio_table)
ratio_table