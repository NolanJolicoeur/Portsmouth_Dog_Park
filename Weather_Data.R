library(glue)
year1 <- 2022
month1 <- 1
day1 <- 5
year2 <- 2022
month2 <- 4 
day2 <- 5
url = glue::glue("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=UUU&data=all&year1={year1}&month1={month1}&day1={day1}&year2={year2}&month2={month2}&day2={day2}&tz=Etc%2FUTC&format=onlycomma&latlon=no&elev=no&missing=M&trace=T&direct=no&report_type=1&report_type=2")
df <- read.csv(url)