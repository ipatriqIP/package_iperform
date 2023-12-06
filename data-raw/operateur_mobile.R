## code to prepare `operateur_mobile` dataset goes here

usethis::use_data(operateur_mobile, overwrite = TRUE)

library(lubridate)
operateur_mobile <- read.csv2("extdata/operateur_mobile.csv", sep = ";", dec = ",", header = TRUE)
operateur_mobile$date <- dmy(operateur_mobile$date)
usethis::use_data(operateur_mobile, overwrite = TRUE)
