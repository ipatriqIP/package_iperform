#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la performance ytd : year-to-date.
#'
#' Cette fonction permet de calculer la somme des valeurs d'une série dans l'année, partant du 1er jour de l'année jusqu'à une date de la même année introduit par l'utilisateur.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date jusqu’à laquelle on vq sommer les valeur de la série depuis le début de l'année.
#' @param a Une valeur de type entier qui permet de décaler l'année par rapport à la date introduite si la série s'etend sur plus d'une année. Sa valeur par defaut est `0`.
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
#' x <- rnorm(102, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2022-12-01"), by = "day", length.out = 102)
#' df <- data.frame(date, x)
#' ytd(df, date = "2023-01-08", x = "x")
#'
ytd <- function(data,
                date,
                a = 0,
                x,
                unite = 1,
                decimal = 0,
                cumul = FALSE) {

  stopifnot(is.data.frame(data), is.character(date) & is.Date(as.Date(date)), is.numeric(a), is.numeric(data[, x]), is.logical(cumul))
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(a) == 1, length(unite) == 1, length(decimal) == 1)

  annee <- year(date) + a
  jour_a <- yday(date)

  data$annee <- year(data[, "date"])
  data$jour_a <- yday(data[, "date"])

  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee + a) & (data[, "jour_a"] == jour_a), x]
    }
  else {
    valeur <- sum(data[(data[, "annee"] == annee) & (data[, "jour_a"] >= 1) & (data[, "jour_a"] <= jour_a), x])
    }

  valeur <- round(valeur/unite, decimal)

  return(valeur)

  }
