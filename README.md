
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iperform <a href="https://ip-ilunga.com"><img src="man/figures/logo.png" align="right" height="138" alt="ip ilunga" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ipatriqIP/package_iperform/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ipatriqIP/package_iperform/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Introduction

Le but de iperform est de permettre aux utilisateurs de calculer les
performances d’une série à une date ou une période donnée. On peut citer
par exemple la performance *month-to-date : mtd* (respectivement
*year-to-date : ytd*) qui consiste à la somme des valeurs de la série
partant du premier jour du mois (respectivement premier jour de l’année
civil) jusqu’à date. Le package permet également de présenter un aperçu
global d’une série différemment de ce qu’on a par exemple avec la
fonction *summary()*.

Apprenez-en plus sur les fonctions de iperform dans
`vignette("iperform")`.

## Installation

L’installation est très simple, depuis CRAN :

``` r
install.packages("iperform", dependencies = TRUE)
```

On peut installer le package depuis ce repository GitHub :

``` r
if(!require(devtools)) {
  install.packages("devtools")
  }
devtools::install_github("ipatriqIP/package_iperform")
```

## Example

Voici un exmple simple sur l’utilisation de la fonction `mtd()` si l’on
veut avoir la performance *month-to-date* en date du *04 août 2023* :

``` r
# importation du package
library(iperform)

## définition des données 
x <- rnorm(222, mean = 50, sd = 6.3)
date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 222)
df <- data.frame(date, x)

# appel de la fonction mtd()
mtd(df, date = "2023-08-04", x = "x", decimal = 2)
#> [1] 200.16
```

Pour verifier, on peut afficher les valeurs qui ont été sommées et
verifier le calcul à la main :

``` r
# mont-to-date au 4 août 2023 signifie les valeurs du 1er août au 4 août iclus
df[(df[, "date"] >= "2023-08-01") & (df[, "date"] <= "2023-08-04"), "x"]
#> [1] 60.28981 39.04513 47.11285 53.71448

# La somme de ces valeurs
round(sum(df[(df[, "date"] >= "2023-08-01") & (df[, "date"] <= "2023-08-04"), "x"]), 2)
#> [1] 200.16
```

## Catégorie des fonctions

Les fonctions de *iperform* se regroupent en 4 catégories :

- “Les performances” qui renvoient les valeurs de la série en une date
  ou une la somme sur une période donnée. On cite les fonctions
  `dday()`, `wtd()`, `mtd()`, `qtd()`, `ytd()` et `full_m()`.

- “Les aperçus” qui renvoient des résumés sur la série en combinant les
  variations comparées aux périodes antérieures de la série. On cite la
  fonction `overview()`

- “Les previsions” qui renvoient les estimations de valeurs de la série
  en des dates futures. On cite la fonction `forecast_m()`.

- “Les transformations” qui renvoient des nouveaux jeu des données créés
  après transformation de la série intiales. On cite la fonction
  `mean_m()`.

## Obtenir de l’aide

Dans le cas où vous rencontrez un bug évident, veuillez déposer un
exemple minimal reproductible sur [github](https://github.com/). Pour
toute question et autre discussion, veuillez utiliser
[community.rstudio.com](https://community.rstudio.com/).
