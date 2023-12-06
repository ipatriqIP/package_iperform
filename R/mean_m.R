#' @importFrom lubridate wday mday yday week month year leap_year is.Date
NULL

#' @importFrom matrixStats rowMins rowMaxs
NULL

#' Calcul de la moyenne mobile sur 7 jour.
#'
#' Cette fonction permet de transformer une série par une nouvelle série dont chaque terme vaut la moyenne de 6 dernier y compris le terme lui même.
#' Cette fonction sera améliorer dans les versions à venir.
#'
#' @param data Un objet de type data.frame contenant au minimum la serie numerique et un vecteur de type date.
#' @param x Un vecteur de type numeric contenant les observations du phénomène à etudier.
#' @param unite Une valeur de type numeric, definissant l'ordre d'echelle d'affisage du resultat de la fonction. Sa valeur par defaut est `1`.
#' @param decimal Un objet de type numeric permettant de préciser le nombre d'entier à afficher après la virgule. Sa valeur par defaut est `0`.
#' @param borne Une valeur de type logic permettant de preciser si les valeurs renvoyées doivent avoir le minimum et le maximum pour chauqe date. Sa valeur par defaut est `TRUE`.
#'
#' @return Un Data.frame content `n + 3` colonnes, dont `n` vaut le nombre disctinct des années que contient la variable date.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' x <- rnorm(222, mean = 50, sd = 6.3)
#' date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 222)
#' df <- data.frame(date, x)
#' mean_m(df, x = "x")
#'
mean_m <- function(data,
                   x,
                   unite = 1,
                   decimal = 0,
                   borne = TRUE) {

  stopifnot(is.data.frame(data), length(unite) == 1, length(decimal) == 1)
  stopifnot("date" %in% colnames(data), x %in% colnames(data))
  stopifnot(is.numeric(data[, x]), is.Date(data[, "date"]))

  data$annee <- year(data[, "date"])
  data$jour_a <- yday(data[, "date"])

  annee <- as.character(unique(data[, "annee"]))
  mat <- matrix(nrow = 365, ncol = length(annee))
  colnames(mat) <- annee

  for (a in annee) {
    n <- yday(max(data[data[, "annee"] == a, "date"]))
    Moy <- c()

    for (j in 1:7) {
      moy <- mean(data[(data[, "annee"] == a) & (data[, "jour_a"] >= 1)  & (data[, "jour_a"] <= j), x])
      Moy <- c(Moy, moy)
      }

    for (i in 8:n) {
      moy <- mean(data[(data[, "annee"] == a) & (data[, "jour_a"] > i-7)  & (data[, "jour_a"] <= i), x])
      Moy <- c(Moy, moy)
      }

    k <- length(Moy)
    ajout_NA <- rep(NA, 365 - k)
    Moy_Mob <- c(Moy, ajout_NA)
    mat[, a] <- Moy_Mob

    }

  d <- as.Date("2000-01-01")
  annee_max <- as.numeric(max(annee))
  lubridate::year(d) <- annee_max

  df_mb <- as.data.frame(round(mat/unite, decimal))
  date <- seq.Date(d, by = "day", length.out = 365)
  Min <- round(rowMins(mat, na.rm = TRUE), decimal)
  Max <- round(rowMaxs(mat, na.rm = TRUE), decimal)
  df_mb <- cbind.data.frame(date, Min, df_mb, Max)

  df_mb = df_mb[df_mb$Min != Inf, ]

  if(isTRUE(borne)) {
    df_mb
    }
  else {
    df_mb[, !colnames(df_mb) %in% c("Min", "Max")]
    }

  }
