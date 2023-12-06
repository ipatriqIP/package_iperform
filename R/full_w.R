#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la performance d'une semaine : full.
#'
#' Cette fonction permet de calculer la somme des valeurs d'une série temporelle au cours d'une semaine par rapport à la date introduit par l'utilisateur.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date à partir de laquelle on determine la semaine à calculer.
#' @param w Une valeur de type entier qui permet de décaler la semaine par rapport à la date introduite. Sa valeur par defaut est `0`.
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
#' full_w(df, date = "2023-01-08", x = "x")
#'
full_w <- function(data,
                   date,
                   w = 0,
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
  data$semaine <- week(data[, "date"])
  data$jour_s <- wday(data[, "date"], week_start = getOption("lubridate.week.start", 1))

  annee <- year(date)
  sem <- week(date) + w

  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee) & (data[, "semaine"] == sem) & (data[, "jour_s"] == 7), x]
    }
  else {
    valeur <- sum(data[(data[, "annee"] == annee) & (data[, "semaine"] == sem), x])
    }

  valeur <- round(valeur/unite, decimal)
  return(valeur)
  }
