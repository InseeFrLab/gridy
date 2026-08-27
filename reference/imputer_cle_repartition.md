# Estimate protected mesh

The information on some mesh cannot be disseminated because it doesn't
respect the threshold rule. For those mesh, an estimation is done by
distributing the total of the variable on a bigger zone (the groupe)
into the meshes, proportionnaly to a given variable (non sensitive
variable).

## Usage

``` r
imputer_cle_repartition(tab_car, list_var_imput, var_cle)
```

## Arguments

- tab_car:

  A data.table of the variables agregates in a grid.

- list_var_imput:

  A character vector giving the names of the variables to estimate.

- var_cle:

  A character, the name of the non-sensitive variable according to which
  the distribution is done.

## Value

The data.table `tab_car` with the estimated variables.

## Examples

``` r
library(data.table)
n <- 1e4
tab <- as.data.table(
  data.frame(id_obs = 1:n, x = rnorm(n, 3e6, 2e4),
  y = rnorm(n, 2e6, 3e4), crs = 3035))
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
tab_car <- tab_GS$tab_car
tab_car[, `:=`(poph = nb_obs*0.48, popf = nb_obs*0.52)]
#>                                 id_carreau                                  p
#>                                     <char>                             <char>
#>     1: FR_CRS3035RES32000mN1920000E2976000                               <NA>
#>     2: FR_CRS3035RES32000mN2016000E2976000                               <NA>
#>     3: FR_CRS3035RES32000mN1984000E2976000                               <NA>
#>     4: FR_CRS3035RES32000mN1952000E2976000                               <NA>
#>     5: FR_CRS3035RES32000mN2016000E3008000                               <NA>
#>    ---                                                                       
#> 10149:  FR_CRS3035RES1000mN2094000E2993000 FR_CRS3035RES2000mN2094000E2992000
#> 10150:  FR_CRS3035RES1000mN2097000E2993000 FR_CRS3035RES2000mN2096000E2992000
#> 10151:  FR_CRS3035RES1000mN2096000E3026000 FR_CRS3035RES2000mN2096000E3026000
#> 10152:  FR_CRS3035RES1000mN2100000E2967000 FR_CRS3035RES2000mN2100000E2966000
#> 10153:  FR_CRS3035RES1000mN2101000E2979000 FR_CRS3035RES2000mN2100000E2978000
#>        niveau nb_obs   etat force groupe    poph    popf
#>         <num>  <int> <lgcl> <num>  <int>   <num>   <num>
#>     1:      1    264   TRUE     0      1  126.72  137.28
#>     2:      1   1330   TRUE     0      2  638.40  691.60
#>     3:      1   2219   TRUE     0      3 1065.12 1153.88
#>     4:      1   1289   TRUE     0      4  618.72  670.28
#>     5:      1    773   TRUE     0      5  371.04  401.96
#>    ---                                                  
#> 10149:      6      1  FALSE     0    400    0.48    0.52
#> 10150:      6      1  FALSE     0    108    0.48    0.52
#> 10151:      6      1  FALSE     0    127    0.48    0.52
#> 10152:      6      1  FALSE     0    130    0.48    0.52
#> 10153:      6      1  FALSE     0    108    0.48    0.52
tab_diff <- imputer_cle_repartition(
  tab_car,
  list_var_imput = c("poph","popf"),
  var_cle = "nb_obs")
```
