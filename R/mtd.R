#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la performance mtd : month-to-date.
#'
#' Cette fonction permet de calculer la somme des valeurs d'une série dans le mois, partant du 1er jour du mois jusqu'à une date du même mois introduit par l'utilisateur.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date jusqu'à laquelle on compte sommer les valeurs au cours du mois.
#' @param m Une valeur de type entier qui permet de décaler le mois par rapport à la date introduite. Sa valeur par defaut est `0`.
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
#' # Voir plus d'exemples et d'explication dans la vignette("iperform")
#'
#' x <- rnorm(222, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 222)
#' df <- data.frame(date, x)
#' mtd(df, date = "2023-08-04", x = "x")
#'
mtd <- function(data,
                date,
                m = 0,
                x,
                unite = 1,
                decimal = 0,
                cumul = FALSE) {

  stopifnot(is.data.frame(data),
            is.character(date) & is.Date(as.Date(date)),
            is.numeric(m),
            is.numeric(data[, x]),
            is.logical(cumul)
            )
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(m) == 1, length(unite) == 1, length(decimal) == 1)

  annee <- year(date)
  jour_m <- mday(date)
  mois <- month(date) + m

  data$annee <- year(data[, "date"])
  data$mois <- month(data[, "date"])
  data$jour_m <- mday(data[, "date"])

  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee) & (data[, "mois"] == mois + m) & (data[, "jour_m"] == jour_m), x]
    }
  else {
    valeur <- sum(data[(data[, "annee"] == annee) & (data[, "mois"] == mois) & (data[, "jour_m"] >= 1) & (data[, "jour_m"] <= jour_m), x])
    }

  valeur <- round(valeur/unite, decimal)

  return(valeur)

  }
