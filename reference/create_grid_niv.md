# Create a square grid

Create a square grid

## Usage

``` r
create_grid_niv(
  tab,
  taille,
  nom_id_car = "id_carreau",
  point_base = c(0, 0),
  eurostat = FALSE
)
```

## Arguments

- tab:

  A data.table with at least a column for x coordinate, a column for y
  coordinate and a column specifying the coordinate reference system
  (crs). For exemple, crs = 3035 for the LAEA projection in Europe.

- taille:

  The size of the mesh in meters.

- nom_id_car:

  A character, the name of the variable for the identifier of the mesh.
  By default it is "id_carreau".

- point_base:

  vector of 2 numeric values, coordinates of the reference point of the
  grid. If eurostat = TRUE, the value used will be automatically c(0,0).

- eurostat:

  boolean, whether the id created has to comply the eurostat inspire
  requirements or not, especially for the conversion in km while using a
  resolution \>= 1000m.

## Value

The data.table `tab` with one more column beeing the identifier of the
mesh of the square grid. The name of the mesh is based on the Inspire
norm : "if the coordinate reference system is projected, the word RES
followed by the grid resolution in meters and the letter m. Then, the
letter N followed by the northing value in meters, and the letter E
followed by the easting value in meters too" (Inspire, Data
Specification for the spatial data theme Statistical Units, 10/12/2013,
page 30). The given position is the position of the lower left cell
corner.

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
res <- create_grid_niv(tab, 200)
```
