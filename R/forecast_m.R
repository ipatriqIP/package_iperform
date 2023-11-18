#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Estimation du total d'un mois sur base des quelques jours observés.
#'
#' Cette fonction permet estimer le total d'un mois sur base de MTD et de nombre de jour restant au courant de ce mois.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date à partir de laquelle on calcule la performance MTD.
#' @param x Un vecteur de type numeric contenant les observations du phénomène à etudier.
#' @param unite Un valeur de type numeric, definissant l'ordre d'echelle d'affisage du resultat de la fonction. Sa valeur par defaut est `1`.
#' @param decimal Un objet de type numeric permettant de préciser le nombre d'entier à afficher après la virgule. Sa valeur par defaut est `0`.
#' @param cumul Un objet de type logic qui permet d'indiquer si le vecteur `x` contient des valeurs cumulées. Sa valeur par defaut est `FALSE`.
#' @param mod Un argument qui permet de modifier le mode de calcul de ce forecast en appliquant un model. Sa valeur par defaut est `NULL`.
#' Pour cette première version, on ne va pas travailler sur le cas où mod n'est pas `NULL`.
#'
#' @return Un vecteur de longieur 1.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' x <- rnorm(102, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2022-12-01"), by = "day", length.out = 102)
#' df <- data.frame(date, x)
#' forecast_m(df, date = "2023-01-25", x = "x")
#'
forecast_m <- function(data,
                       date,
                       x,
                       unite = 1,
                       decimal = 0,
                       cumul = FALSE,
                       mod = "NULL") {

  stopifnot(is.data.frame(data),
            is.character(date) & is.Date(as.Date(date)),
            is.numeric(data[, x]),
            is.logical(cumul),
            is.character(mod)
            )
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(unite) == 1, length(decimal) == 1, length(mod) == 1)

  annee <- year(date)
  mois <- month(date)

  if (isTRUE(leap_year(annee)) & mois == 2) {
    nb_jr <- 29
    }
  else {
    nb_jr <- ifelse(mois %in% c(4, 6, 9, 11), 30,
                    ifelse(mois == 2, 28, 31))
    }

  jour_m <- mday(date)
  jour_r <- nb_jr - jour_m
  MTD <- mtd(data, date = date, x = x, cumul = cumul)

  if (mod == "NULL") {
    valeur <- MTD + (MTD/jour_m*jour_r)
    }
  else {
    valeur <- MTD + (MTD/jour_m*jour_r)
    }

  valeur <- round(valeur/unite, decimal)

  return(valeur)

  }
