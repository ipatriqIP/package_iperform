#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la performance d'une trimestre : full.
#'
#' Cette fonction permet de calculer la somme des valeurs d'une série temporelle au cours d'un trimestre par rapport à la date introduit par l'utilisateur.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date à partir de laquelle on determine l’année à calculer.
#' @param q Une valeur de type entier qui permet de décaler le trimestre par rapport à la date introduite. Sa valeur par defaut est `0`.
#' @param x Un vecteur de type numeric contenant les observations du phénomène observé.
#' @param unite Une valeur de type numeric, definissant l'ordre d'echelle d'affisage de résultat de la fonction. Sa valeur par defaut est `1`.
#' @param decimal Un objet de type numeric permettant de préciser le nombre d'entier à afficher après la virgule. Sa valeur par defaut est `0`.
#' @param cumul Un objet de type logic qui permet d'indiquer si le vecteur `x` contient des valeurs cumulées. Sa valeur par defaut est `FALSE`.
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
#' full_q(df, date = "2023-01-08", x = "x")
#'
full_q <- function(data,
                   date,
                   q = 0,
                   x,
                   unite = 1,
                   decimal = 0,
                   cumul = FALSE) {

  stopifnot(is.data.frame(data),
            is.character(date) & is.Date(as.Date(date)),
            is.numeric(data[, x]),
            is.logical(cumul))
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(unite) == 1, length(decimal) == 1)

  data$annee <- year(data[, "date"])
  data$trimestre <- quarter(data[, "date"])
  data$jour_a <- yday(data[, "date"])

  annee <- year(date)
  trim <- quarter(date) + q

  b <- ifelse(leap_year(annee), 1, 0)
  fin_trim <- ifelse(trim == 1, 90 + b, ifelse(trim == 2, 180 + b, ifelse(trim == 3, 273 + b, 365 + b)))

  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee) & (data[, "trimestre"] == trim) & (data[, "jour_a"] == fin_trim), x]
    }
  else {
    valeur <- sum(data[(data[, "annee"] == annee) & (data[, "trimestre"] == trim), x])
    }

  valeur <- round(valeur/unite, decimal)
  return(valeur)
  }
