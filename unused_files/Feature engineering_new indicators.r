###########################################################################################################
library(dplyr)
library(here)
library(tidyr)
library(data.table)
orders_allyears_df <- readRDS(here("intermediary_data/orders_allyears_df.rds"))

# Add indicators for the day of the week, month and season
orders_allyears_df$Weekday <- weekdays(as.Date(orders_allyears_df$`Date Received`,"%Y/%m/%d"))
orders_allyears_df$Month <- format(as.Date(orders_allyears_df$`Date Received`,"%Y/%m/%d"),"%m")
getSeason <- function(Dates){
   Month <- as.numeric(format(as.Date(Dates, "%Y/%m/%d"),"%m"))
  ifelse(Month >= 12 | Month < 03,
         "Winter",
        ifelse(Month >= 03 & Month < 06,
               "Spring",
                ifelse(Month >= 06 & Month < 09,
                      "Summer",
                      "Fall")))
}
orders_allyears_df$Season <- getSeason(as.Date(orders_allyears_df$`Date Received`,"%Y/%m/%d"))

# Add indicator for the pre/post Covid status
getCovidStatus <- function(Dates){
  date <- as.Date(Dates,"%Y/%m/%d")
  Covid_date <- as.Date("2020/03/12", "%Y/%m/%d")
  ifelse(date < Covid_date, FALSE, TRUE)
}
orders_allyears_df$Post_covid <- getCovidStatus(orders_allyears_df$`Date Received`)

# Add indicators for the school year and the first week of the semester
# The school year and first week of each semester were determined based on the Wake County Public School System
# Reference: 2018-2022 years' traditional calender, link: 'https://www.wcpss.net/calendars'

getSchoolYear <- function(Dates){
  date <- as.Date(Dates,"%Y/%m/%d")
  schoolyear2018_e <- as.Date("2019/06/11", "%Y/%m/%d")
  schoolyear2019_b <- as.Date("2019/08/26", "%Y/%m/%d")
  schoolyear2019_e <- as.Date("2020/06/12", "%Y/%m/%d")
  schoolyear2020_b <- as.Date("2020/08/17", "%Y/%m/%d")
  schoolyear2020_e <- as.Date("2021/06/10", "%Y/%m/%d")
  schoolyear2021_b <- as.Date("2021/08/23", "%Y/%m/%d")
  ifelse(date %between% c(schoolyear2018_e,schoolyear2019_b), FALSE,
         ifelse(date %between% c(schoolyear2019_e,schoolyear2020_b), FALSE,
                ifelse(date %between% c(schoolyear2020_e,schoolyear2021_b), FALSE, TRUE)))
}
orders_allyears_df$School_year <- getSchoolYear(orders_allyears_df$`Date Received`)

getFirstWeek <- function(Dates){
  date <- as.Date(Dates,"%Y/%m/%d")
  ifelse(date %between% c("2019/08/25", "2019/08/31"), TRUE,
         ifelse(date %between% c("2020/01/05", "2020/01/11"), TRUE,
                ifelse(date %between% c("2020/08/16", "2020/08/22"), TRUE,
                       ifelse(date %between% c("2021/01/03", "2021/01/09"), TRUE,
                              ifelse(date %between% c("2021/08/22", "2021/08/28"), TRUE, FALSE)))))
}
orders_allyears_df$Firstweek <- getFirstWeek(orders_allyears_df$`Date Received`)

# Check the data set
table(orders_allyears_df$Weekday)
table(orders_allyears_df$Month)
table(orders_allyears_df$Season)
table(orders_allyears_df$Post_covid)
table(orders_allyears_df$School_year)
table(orders_allyears_df$Firstweek)

# Save the edited data set
saveRDS(orders_allyears_df, file = here("intermediary_data/orders_allyears_df_FE.rds"))
