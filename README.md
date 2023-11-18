
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iperform <a href="https://ip-ilunga.com"><img src="man/figures/logo.png" align="right" height="138" alt="ip ilunga" /></a>

<!-- badges: start -->
<!-- badges: end -->

## Introduction

Le but de iperform est de permettre aux utilisateurs de calculer les
performances d’une série à une date (ou une période) donnée. On peut
citer par exemple la performance *month-to-date : mtd* (respectivement
*year-to-date : ytd*) qui consiste à la somme des valeurs de la série
partant du premier jour du mois (respectivement premier jour de l’année
civil) jusqu’à date. Le package permet aussi à présenter des aperçu
global d’une série différemment de ce que presente par exemple les
fonctions *summary()* et *summarise()*.

Apprenez-en plus sur les fonctions de iperform dans
`vignette("iperfom")`.

## Installation

L’installation est très simple, comme le montre les codes ci-dessous :

``` r
install.packages("iperform")
```

## Example

Voici un exmple simple sur l’utilisation de la fonction `mtd`:

``` r
# importation du package
library(iperform)

## définition des données 
x <- rnorm(222, mean = 50, sd = 6.3)
date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 222)
df <- data.frame(date, x)

# appel de la fonction mtd()
mtd(df, date = "2023-08-04", x = "x", decimal = 2)
#> [1] 212.41
```

Pour verifier, on peut afficher les valeurs qui ont été sommées et
verifier le calcul à la main :

``` r
# mont-to-date au 4 août 2023 signifie les valeurs du 1er août au 4 août iclus
df[(df[, "date"] >= "2023-08-01") & (df[, "date"] <= "2023-08-04"), "x"]
#> [1] 61.03714 55.75847 56.64705 38.97114
```

## Catégorie des fonctions

Les fonctions de *iperform* se regroupent en 4 catégories :

- “les performances” qui renvoient les valeurs de la série en une date
  ou une la somme sur une période donnée. On cite `dday()`, `wtd()`,
  `mtd()`, `ytd()` et `full_m()`.

- “les aperçus” qui renvoient des résumés sur la série en combinant les
  variations comparées aux périodes antérieures de la série. On cite
  `overview()`

- “les previsions” qui renvoient les estimations de valeurs de la série
  en des dates futures. On cite `forecast_m()`.

- “les transformations” qui renvoient des nouveaux jeu des données créés
  après transformation de la série intiales. On cite `m_mean()`,
  `rva()`.

## Obtenir de l’aide

Dans le cas où vous rencontrez un bug évident, veuillez déposer un
exemple minimal reproductible sur [github](https://github.com/). Pour
toute question et autre discussion, veuillez utiliser
[community.rstudio.com](https://community.rstudio.com/).
