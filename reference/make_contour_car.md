# Creates a polygon from tile's ID

Creates a polygon from tile's ID

## Usage

``` r
make_contour_car(id_car)
```

## Arguments

- id_car:

  id of tile, in INSPIRE style

## Value

sf object

## Examples

``` r
make_contour_car("CRS3035RES200mN2009400E2992400")
#> POLYGON ((2992400 2009400, 2992400 2009600, 2992600 2009600, 2992600 2009400, 2992400 2009400))
```
