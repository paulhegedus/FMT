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
  
  ## make day and hour combination
  hour_f <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
              "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", 
              "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", 
              "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
  day_f <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  temp <- as.list(day_f) %>% 
    lapply(function(x, hour_f) paste(x, hour_f), hour_f) %>% 
    unlist()
  gsheet$day_hour <- paste(gsheet$weekday_f, gsheet$hour_f) %>% 
    factor(levels = temp)
  
  return(gsheet)
}


## calcFreq : Calculate frequency of email/post across month, week, day
## Args : Google Sheet Import from '2022 Communications Metrics' that 
##        has been run through the fixDayDateTime() function.
## Method : Creates columns to calculate the following; 
##          Create 'month_freq' = Number of email/posts that month.
##          Create 'week_freq' = Number of email/posts that week.
##          Create 'day_freq' = Number of email/posts that day.
## Output : Google Sheet with appended columns.
calcFreq <- function(gsheet) {
  gsheet$id <- 1:nrow(gsheet)
  # calculate the number of emails/posts in a month
  temp <- by(gsheet, gsheet$month_f, nrow) %>% lapply(function(x) x[1]) %>% unlist() %>% as.data.frame()
  names(temp) <- "month_freq"
  temp$month <- row.names(temp)
  temp2 <- merge(gsheet, temp, by = "month")[, c("id", "month_freq")]
  gsheet$month_freq <- NA
  gsheet[temp2$id, "month_freq"] <- temp2$month_freq
  
  # calculate the number of emails/posts in a week
  gsheet$week <- strftime(gsheet$Date, format = "%V")
  temp <- by(gsheet, gsheet$week, nrow) %>% lapply(function(x) x[1]) %>% unlist() %>% as.data.frame()
  names(temp) <- "week_freq"
  temp$week <- row.names(temp)
  temp2 <- merge(gsheet, temp, by = "week")[, c("id", "week_freq")]
  gsheet$week_freq <- NA
  gsheet[temp2$id, "week_freq"] <- temp2$week_freq

  # calculate the number of emails/posts in a day
  temp <- by(gsheet, gsheet$usa_date, nrow) %>% lapply(function(x) x[1]) %>% unlist() %>% as.data.frame()
  names(temp) <- "day_freq"
  temp$usa_date <- row.names(temp)
  temp2 <- merge(gsheet, temp, by = "usa_date")[, c("id", "day_freq")]
  gsheet$day_freq <- NA
  gsheet[temp2$id, "day_freq"] <- temp2$day_freq
  
  return(gsheet)
}

