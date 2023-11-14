## code to prepare `kpi_voix_op_tel_mob` dataset goes here

usethis::use_data(kpi_voix_op_tel_mob, overwrite = TRUE)

library(lubridate)

voix_tel_mob = read.csv2("extdata/kpi_voix_op_tel_mob.csv", sep = ";", dec = ",", header = TRUE)
voix_tel_mob$date = dmy(voix_tel_mob$date)

usethis::use_data(voix_tel_mob, overwrite = TRUE)
