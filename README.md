
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![minimum R version](https://img.shields.io/badge/R%3E%3D-3.5-blue.svg)](https://gitlab.insee.fr/outilsconfidentialite/ckm/-/blob/main/DESCRIPTION)
[![pipeline status](https://gitlab.insee.fr/outilsconfidentialite/ckm/badges/main/pipeline.svg)](https://gitlab.insee.fr/outilsconfidentialite/ckm/-/pipelines)

# Appliquer la méthode des clés aléatoires


## Installation du package

La procédure d'installation dépend de l'environnement dans lequel se trouve `R`

<!--
 __Accès à internet: depuis `Github`__

```r
# install.packages("remotes")
remotes::install_github("inseefrlab/ckm",
                     dependencies = TRUE)
                     
```
-->

__Pas d'accès à internet (AUS, LS3...): depuis un `Gitlab` interne :

```r
# install.packages("remotes")
remotes::install_git("https://gitlab.insee.fr/outilsconfidentialite/ckm.git",
                     dependencies = TRUE)
                     
```


## Appliquer la méthode des clés aléatoires étape par étape

### 1-Poser une clé aléatoire sur le jeu de données individuelles

La commande `set.seed()` permet de fixer une graine aléatoire et d'assurer 
la reproductibilité du code, en l'occurrence ici du tirage des clés 
individuelles.

```r
library(ckm)

data("dtest", package = "ckm")

set.seed(4081789)
dtest_avec_cles <- construire_cles_indiv(dtest)
hist(dtest_avec_cles$rkey)
                     
```


### 2-Générer le tableau de comptages avec la clé des cellules


```r
tab_avant <- tabulate_cnt_micro_data(
 df = dtest_avec_cles,
 cat_vars = c("DIPLOME", "SEXE", "AGE"),
 hrc_vars = list(GEO = c("REG", "DEP")),
 marge_label = "Total"
)
                     
```

### 3-Appliquer la perturbation

```r
res_ckm <- appliquer_ckm(tab_avant, D = 5, V = 2)

```


### 4-Mesurer la perte d'information

En cours de dev

### 5- Mesure le risque

EN cours de dev

## Appliquer la méthode des clés aléatoires en une étape

Après avoir généré la clé individuelle sur votre jeu de données, il est possible
de construire directement le tableau perturbé


```r
res_ckm <- tabuler_et_appliquer_ckm(
 df = dtest_avec_cles,
 cat_vars = c("DIPLOME", "SEXE", "AGE"),
 hrc_vars = list(GEO = c("REG", "DEP")),
 marge_label = "Total",
 D = 5, V = 2
)
str(res_ckm$tab)                   
```
