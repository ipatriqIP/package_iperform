#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la performance d'un mois : full.
#'
#' Cette fonction permet de calculer la somme des valeurs d'une série dans le mois par rapport au mois correspondant à la date introduit par l'utilisateur.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date à partir de laquelle on determine le mois à calculer.
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
#' full_m(df, date = "2023-01-08", x = "x")
#'
full_m <- function(data,
                   date,
                   x,
                   unite = 1,
                   decimal = 0,
                   cumul = FALSE) {

  stopifnot(is.data.frame(data),
            is.character(date) & is.Date(as.Date(date)),
            is.numeric(data[, x]),
            is.logical(cumul)
            )
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(unite) == 1, length(decimal) == 1)

  annee <- year(date)
  mois <- month(date)

  if (isTRUE(leap_year(annee)) & mois == 2) {
    nb_jr <- 29
    }
  else {
    nb_jr <- ifelse(mois %in% c(4, 6, 9, 11), 30,
                    ifelse(mois == 2, 28, 31))
    }

  data$annee <- year(data[, "date"])
  data$mois <- month(data[, "date"])
  data$jour_m <- mday(data[, "date"])

  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee) & (data[, "mois"] == mois) & (data[, "jour_m"] == nb_jr), x]
    }
  else {
    valeur <- sum(data[(data[, "annee"] == annee) & (data[, "mois"] == mois) & (data[, "jour_m"] >= 1) & (data[, "jour_m"] <= nb_jr), x])
    }

  valeur <- round(valeur/unite, decimal)

  return(valeur)

  }
