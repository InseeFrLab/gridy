# Create several square grids

Create several square grids

## Usage

``` r
create_grids(tab, mailles, eurostat = FALSE)
```

## Arguments

- tab:

  A data.table with at least a column for x coordinate, a column for y
  coordinate and a column specifying the coordinate reference system
  (crs). For exemple, crs = 3035 for the LAEA projection in Europe.

- mailles:

  A vector of numbers indicating the sizes of the mesh of the different
  grid in a decreasing order.If mailles has names, they will be used to
  create the name of the variable.

- eurostat:

  boolean, whether the id created has to comply the eurostat inspire
  requirements or not, especially for the conversion in km while using a
  resolution \>= 1000m.

## Value

The data.table `tab` with one more column for each grid. The names of
these column are "id_carreau_nivX" where X stands for the level of the
grid in decreasing order (level 1 for the bigger grid).

## Examples

``` r
library(data.table)
tab <- as.data.table(
  data.frame(
    id_obs = 1:10,
    x = rnorm(10,3e6,1e4),
    y = rnorm(10, 2e6, 1e4),
    crs = 3035)
)
grids1 <- create_grids(tab, c(1000,200))
grids2 <- create_grids(tab, c("1km" = 1000, "200m" = 200))
```
