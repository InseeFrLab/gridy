# determine the natural grid

determine the natural grid

## Usage

``` r
determiner_car_naturel(resul_GS)
```

## Arguments

- resul_GS:

  result of the create_GS_CPP() function

## Value

a data.table dataframe

## Examples

``` r
library(data.table)
n <- 1e4
tab <- as.data.table(
  data.frame(
    id_obs = 1:n,
    x = rnorm(n, 3e6, 2e4),
    y = rnorm(n, 2e6, 3e4),
    crs = 3035)
)
tab_GS <- create_GS_CPP(tab, 5, c(32e3,16e3,8e3,4e3,2e3,1e3))
#> [1] "Etape 1 : creation des differentes grilles *"
#> [1] "Etape 2 : Initialiser la table de diffusion 'tdiff' **"
#> [1] "Etape 3 : On complete la table de diffusion, en parcourant chaque carreau, du plus grand au plus petit ***"
#> [1] "      Traitement des carreaux de niveau 1"
#> 
#> [1] "      Traitement des carreaux de niveau 2"
#> 
#> [1] "      Traitement des carreaux de niveau 3"
#> 
#> [1] "      Traitement des carreaux de niveau 4"
#> 
#> [1] "      Traitement des carreaux de niveau 5"
#> 
tab_car_nat <- determiner_car_naturel(tab_GS)
#> [1] "fin du niveau 1"
#> [1] "fin du niveau 2"
#> [1] "fin du niveau 3"
#> [1] "fin du niveau 4"
#> [1] "fin du niveau 5"
```
