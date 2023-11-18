#' @importFrom lubridate wday mday yday week month quarter year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la valeur qtd d'une serie.
#'
#' Cette fonction permet de trouver la valeur d'une série à une date voulue par l'utilisateur, le d-day.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date que l'on souhaite connaitre la valeur de la serie.
#' @param q Une valeur de type entier qui permet de décaler le trimestre par rapport à la date introduite. Sa valeur par defaut est `0`.
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
#' date <- seq.Date(as.Date("2023-01-01"), by = "day", len = 222)
#' df <- data.frame(date, x)
#' qtd(df, date = "2023-07-06", x = "x")
#'
qtd <- function(data,
                date,
                q = 0,
                x,
                unite = 1,
                decimal = 0,
                cumul = FALSE) {

  stopifnot(is.data.frame(data),
            is.character(date) & is.Date(as.Date(date)),
            is.numeric(q),
            is.numeric(data[, x]),
            is.logical(cumul)
            )
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(length(date) == 1, length(unite) == 1, length(decimal) == 1)

  annee <- year(date)
  trimestre <- quarter(date) + q

  b <- ifelse(leap_year(annee), 1, 0)
  debut_trim <- ifelse(trimestre == 1, 1,
                       ifelse(trimestre == 2, 91 + b,
                              ifelse(trimestre == 3, 182 + b, 274 + b)))

  jour_a <- yday(date)

  data$annee <- year(data[, "date"])
  data$trimestre <- quarter(data[, "date"])
  data$jour_a <- yday(data[, "date"])


  if (isTRUE(cumul)) {
    valeur <- data[(data[, "annee"] == annee) & (data[, "trimestre"] == trimestre + q) & (data[, "jour_a"] == jour_a), x]
    }
  else {
    valeur <- sum(data[(data[, "annee"] == annee) & (data[, "trimestre"] == trimestre) & (data[, "jour_a"] >= debut_trim) & (data[, "jour_a"] <= jour_a), x])
    }

  valeur <- round(valeur/unite, decimal)

  return(valeur)
  }
