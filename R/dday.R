#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la valeur d-day d'une serie.
#'
#' Cette fonction permet de trouver la valeur d'une série à une date voulue par l'utilisateur, le d-day.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date que l'on souhaite connaitre la valeur de la serie.
#' @param d Une valeur de type entier qui permet de décaler le jour par rapport à la date introduite. Sa valeur par defaut est `0`.
#' @param x Un vecteur de type numeric contenant les observations du phénomène à etudier.
#' @param unite Un valeur de type numeric, definissant l'ordre d'echelle d'affisage du resultat de la fonction. Sa valeur par defaut est `1`.
#' @param decimal Un objet de type numeric permettant de préciser le nombre d'entier à afficher après la virgule. Sa valeur par defaut est `0`.
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
#' dday(df, date = "2023-07-06", x = "x")
#'
dday <- function(data,
                 date,
                 d = 0,
                 x,
                 unite = 1,
                 decimal = 0) {

  stopifnot(is.data.frame(data),
            is.character(date) & is.Date(as.Date(date)),
            is.numeric(d),
            is.numeric(data[, x])
            )
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(d) == 1, length(unite) == 1, length(decimal) == 1)

  valeur <- data[data[, "date"] == as.Date(date) + d, x]
  valeur <- round(valeur/unite, decimal)

  return(valeur)
  }
