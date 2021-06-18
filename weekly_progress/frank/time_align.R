## Time alignment
library(dplyr)
library(econR)
library(lubridate)
googleLink_download("https://drive.google.com/file/d/1Y-MY2oGCe2RSQ99JdYyHwYyR-EjLR1Ei/view?usp=sharing")
taskdata = readRDS("/Users/frankchao/Desktop/109-2-app101/weekly_progress/frank/taskdata.Rds")

# Write the following function, so that 


taskdata$before %>%
  align_date(
    date_colname = "date",
    freq = "yearly"
  )


# will return an outcome like 

taskdata$after


taskdata$before %>% 
  mutate(
    date = ymd(c("2001-03-01","2001-04-01","2001-07-01","2003-02-01","2003-05-01"))
  ) -> month_test

taskdata$before %>% 
  mutate(
    date = ymd(c("2001-11-21","2001-11-30","2002-01-1","2002-01-19","2003-02-10"))
  ) -> daily_test



#' Align data frame date column
#'
#' @param df A data frame
#' @param date A character of colname name in df that represents date (which is a Date class)
#' @param freq A character of "daily", "monthly", "quarterly" or "yearly".
#'
#' @return
#' @export
#'
#' @examples none

align_date <- function(df,date_colname,freq="daily"){
      switch(
          freq,
          yearly = list(
                         x1 = 365,
                         x2 = years   
                   ),
          daily = list(
                         x1 = 1,
                         x2 = days
                   ),
          monthly = list(
                         x1 = 30,
                         x2 = months
                   )
      ) -> v
      count <- count_by_freq(df,date_colname,v$x1)
      new_date <- rep(df[[date_colname]][1],count)
      for(d in seq_along(new_date)[-1]){
         new_date[d] = new_date[d-1]+v$x2(1)
      }
      data.frame(date = new_date) %>% left_join(df) %>%
      return()     
}

# test

align_date(taskdata$before,"date",freq = "yearly")
align_date(month_test,"date",freq = "monthly")
align_date(daily_test,"date",freq = "daily")

# helper

count_by_freq <- function(df,date_colname,days){
    ((df[[date_colname]][length(df[[date_colname]])] - 
      df[[date_colname]][1])/days)+1 %>%
      as.integer() %>% round() %>% return()
}


