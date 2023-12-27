#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de taux de variation entre les observations d'une serie.
#'
#' Cette fonction permet de calculer le taux de variation entre les valeurs d'une série à une date ou une periode.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date de l'observation d’arrivée.
#' @param x Un vecteur de type numeric contenant les observations du phénomène à etudier.
#' @param variation Un objet de type character, definissant le type de variation à calculer. Sa valeur par defaut est `dtd`.
#' @param p Une valeur de type entier qui permet de décaler le jour pour une variation journalière. Sa valeur par defaut est `-7`.
#' @param normaliz Un objet de type logic, permettant de preciser si la variation doit se faire à base egale pour des périodes. Sa valeur par defaut est `FALSE`.
#' @param decimal Un objet de type numeric permettant de préciser le nombre d'entier à afficher après la virgule. Sa valeur par defaut est `2`.
#'
#' @return Un vecteur de longieur 1.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' x <- rnorm(222, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2023-01-01"), by = "day", len = 222)
#' df <- data.frame(date, x)
#' taux_v(df, date = "2023-04-30", x = "x")
#'
taux_v <- function(data, date, x, variation = "dtd", p = -7, normaliz = FALSE, decimal = 2) {

  tv <- switch(variation,
              "dtd" = dday(data = data, date = date, x = x)/dday(data = data, date = date, x = x, d = p) - 1,
              "wtd" = wtd(data = data, date = date, x = x)/wtd(data = data, date = date, w = -1, x = x) - 1,
              "mtd" = mtd(data = data, date = date, x = x)/mtd(data = data, date = date, m = -1, x = x) - 1,
              "qtd" = qtd(data = data, date = date, x = x)/qtd(data = data, date = date, q = -1, x = x) - 1,
              "htd" = htd(data = data, date = date, x = x)/htd(data = data, date = date, h = -1, x = x) - 1,
              "ytd" = ytd(data = data, date = date, x = x)/ytd(data = data, date = date, a = -1, x = x) - 1
              )

  tv <- round(tv*100, decimal)

  return(tv)
  }
