#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de l’aperçu global : overview.
#'
#' Cette fonction permet de donner un résumé sur les performances d'une série par rapport à une date introduit par l'utilisateur.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param date La date jusqu’à laquelle on vq sommer les valeur de la série depuis le début de l'année.
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
#' x <- rnorm(600, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = 600)
#' df <- data.frame(date, x)
#' overview(df, date = "2023-05-30", x = "x")
#' overview(df, date = "2023-04-25", x = "x", decimal = 2)
#'
overview <- function(data,
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

  Facteur <- x

  DDay <- dday(data = data, date = date, d = 0, x = x, unite = unite)
  DDay7 <- dday(data = data, date = date, d = -7, x = x, unite = unite)
  WTD <- wtd(data = data, date = date, w = 0, x = x, unite = unite, cumul = cumul)
  WTD1 <- wtd(data = data, date = date, w = -1, x = x, unite = unite, cumul = cumul)
  MTD <- mtd(data = data, date = date, m = 0, x = x, unite = unite, cumul = cumul)
  MTD1 <- mtd(data = data, date = date, m = -1, x = x, unite = unite, cumul = cumul)
  Full_M <- full_m(data = data, date = date, x = x, unite = unite, cumul = cumul)
  FORECAST <- forecast_m(data = data, date = date, x = x, unite = unite)
  YTD <- ytd(data = data, date = date, a = 0, x = x, unite = unite, cumul = cumul)
  YTD1 <- ytd(data = data, date = date, a = -1, x = x, unite = unite, cumul = cumul)

  DoD <- round((DDay/DDay7 - 1)*100, decimal)
  WoW <- round((WTD/WTD1 - 1)*100, decimal)
  SPLM <- round((MTD/MTD1 - 1)*100, decimal)
  MoM <- round((FORECAST/Full_M - 1)*100, decimal)
  SPLY <- round((YTD/YTD1 - 1)*100, decimal)

  df <- data.frame(Facteur, DDay7, DDay, DoD, WTD1, WTD, WoW, MTD1, MTD, SPLM, Full_M, FORECAST, MoM, YTD1, YTD, SPLY)
  df[is.na(df)] <- 0

  return(df)

  }
