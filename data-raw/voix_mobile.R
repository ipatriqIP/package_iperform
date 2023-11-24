## code to prepare `voix_mobile` dataset goes here

library(lubridate)

voix_mobile <- read.csv2("extdata/voix_mobile.csv", sep = ";", dec = ",", header = TRUE)
voix_mobile$date <- dmy(voix_mobile$date)

usethis::use_data(voix_mobile, overwrite = TRUE)
