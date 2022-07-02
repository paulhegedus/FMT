## Functions for FMT Comms Analysis
library(tidyverse)

## fixDayDateTime : Format 'Weekday', 'Date', and 'Time' columns
## Args : Google Sheet Import from '2022 Communications Metrics' 
##        w/ Weekday, Date, and Time in character (c), date (D), 
##        and time (t) formats.
## Method : Creates columns to format the following; 
##          Create 'ampm_time' = 'Time' to HH:MM AM/PM.
##          Create 'usa_date' = 'Date' to MM/DD/YYYY.
##          Create 'weekday_f' = 'Weekday' to factor.
##          Create 'month' = Month of email/post
##          Create 'hour' = Hour of post.
## Output : Google Sheet with appended columns.
fixDayDateTime <- function(gsheet) {
  # convert time to something readable
  gsheet$ampm_time <- hms::as_hms(gsheet$Time) %>%
    as.character() %>% 
    strptime("%H:%M:%S") %>% 
    format(format = "%I:%M %p")
  # make date in the same format as OG (MM/DD/YYYY)
  gsheet$usa_date <- format(
    gsheet$Date, 
    format = "%m/%d/%Y"
  )
  # make Weekday factor
  gsheet$weekday_f <- factor(
    gsheet$Weekday, 
    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  )
  # make month from usa_date
  gsheet$month <- months(gsheet$Date)
  gsheet$month_f <- factor(
    gsheet$month,
    levels = c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")
  )
  # make hour from ampm_time
  hours <- str_split(gsheet$ampm_time, ":") %>% 
    lapply(function(x) return(x[1])) %>% 
    unlist()
  ampm <- str_split(gsheet$ampm_time, " ") %>% 
    lapply(function(x) return(x[2])) %>% 
    unlist()
  gsheet$hour <- paste(hours, ampm)
  gsheet$hour <- str_replace_all(gsheet$hour, "^0", "")
  gsheet$hour <- ifelse(gsheet$hour == "NA NA", NA, gsheet$hour)
  # make factor
  gsheet$hour_f <- factor(
    gsheet$hour,
    levels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
               "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", 
               "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", 
               "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
  )
  
  return(gsheet)
}

