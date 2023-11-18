#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la performance wtd : week-to-date.
#'
#' Cette fonction permet de calculer la somme des valeurs d'une série dans la semaine, partant du 1er jour de la semaine jusqu'à la date introduit par l'utilisateur.
#' Pour cette première version, on garde par defaut dimanche comme premier jour de la semaine.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date que l'on souhaite connaitre la valeur de la serie.
#' @param w Une valeur de type entier qui permet de décaler la semaine par rapport à la date introduite. Sa valeur par defaut est `0`.
#' @param x Un vecteur de type numeric contenant les observations du phénomène à etudier.
#' @param unite Un valeur de type numeric, definissant l'ordre d'echelle d'affisage du resultat de la fonction. Sa valeur par defaut est `1`.
#' @param decimal Un objet de type numeric permettant de préciser le nombre d'entier à afficher après la virgule. Sa valeur par defaut est `0`.
#' @param cumul Un objet de type logic qui permet d'indiquer si le vecteur `x` contient des valeurs cumulées. Sa valeur par defaut est `FALSE`.
#'
#' @return Un vecteur de longieur 1.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' x <- rnorm(222, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 222)
#' df <- data.frame(date, x)
#' wtd(df, date = "2023-08-01", x = "x")
#'
wtd <- function(data,
                date,
                w = 0,
                x,
                unite = 1,
                decimal = 0,
                cumul = FALSE) {

  stopifnot(is.data.frame(data), is.character(date) & is.Date(as.Date(date)), is.numeric(w), is.numeric(data[, x]), is.logical(cumul))
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(w) == 1, length(unite) == 1, length(decimal) == 1)

  annee <- year(date)
  num_s <- week(date) + w
  num_j <- wday(date, week_start = getOption("lubridate.week.start", 7))

  data$annee <- year(data[, "date"])
  data$semaine <- week(data[, "date"])
  data$jour_s <- wday(data[, "date"], week_start = getOption("lubridate.week.start", 7))

  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee) & (data[, "semaine"] == num_s + w) & (data[, "jour_s"] == num_j), x]
    }
  else {
    valeur <- sum(data[((data[, "annee"] == annee) & (data[, "semaine"] == num_s) & (data[, "jour_s"] >= 1) & (data[, "jour_s"] <= num_j)), x])
    }

  valeur <- round(valeur/unite, decimal)

  return(valeur)

  }
