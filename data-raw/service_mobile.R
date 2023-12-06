## code to prepare `service_mobile` dataset goes here

usethis::use_data(service_mobile, overwrite = TRUE)

library(lubridate)
service_mobile <- read.csv2("extdata/service_mobile.csv", sep = ";", dec = ",", header = TRUE)
service_mobile$date <- dmy(service_mobile$date)
usethis::use_data(service_mobile, overwrite = TRUE)
