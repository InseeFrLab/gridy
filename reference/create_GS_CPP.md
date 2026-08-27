# Create confidential grids

The confidentiality rule is not to have meshs containing less
observations than the threshold.

## Usage

``` r
create_GS_CPP(tab, seuil, mailles, agreg = FALSE, ...)
```

## Arguments

- tab:

  A data.table at individual level with at least (x, y, crs) columns or
  an aggregated data.table with at least (n_obs, x, y, crs). In this
  latter case, observations can be aggregated on the centroid of the
  squares of the finest grid level.

- seuil:

  The confidentiality threshold.

- mailles:

  A vector with the sizes of the different grids.

- agreg:

  Boolean to mention if tab is already an agreggated table.

- ...:

  other arguments from
  [`create_grid_niv()`](https://inseefrlab.github.io/gridy/reference/create_grid_niv.md)

## Value

At the end of the process, we get a data.table with one row for each
mesh of each grid. In the columns we have the identifier of the mesh,
the identifier of the mesh at the above level, the number of observation
in the mesh, the state of the mesh (can the information on the mesh be
disseminated without perturbation or not), the force of the mesh and the
group of the mesh.

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
n <- 1e4
tab <- as.data.table(
  data.frame(id_obs = 1:n, x = rnorm(n, 3e6, 2e4),
  y = rnorm(n, 2e6, 3e4), crs = 3035))
tab_grid <- create_GS_CPP(tab, 5, c(32e3,16e3,8e3,4e3,2e3,1e3))
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
```
