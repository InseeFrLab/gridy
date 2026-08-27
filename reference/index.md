# Package index

## Main function

The entry point of the package. Overlays the nested grids, walks down
the quadtree and returns the dissemination table holding the state,
force and group of every cell at every level.

- [`create_GS_CPP()`](https://inseefrlab.github.io/gridy/reference/create_GS_CPP.md)
  : Create confidential grids

## Building grids

Creating square grids and their Inspire identifiers. Useful on their
own, and called internally by
[`create_GS_CPP()`](https://inseefrlab.github.io/gridy/reference/create_GS_CPP.md).

- [`create_grid_niv()`](https://inseefrlab.github.io/gridy/reference/create_grid_niv.md)
  : Create a square grid
- [`create_grids()`](https://inseefrlab.github.io/gridy/reference/create_grids.md)
  : Create several square grids

## Extracting the natural grid

Deriving the variable-resolution grid from the output of
[`create_GS_CPP()`](https://inseefrlab.github.io/gridy/reference/create_GS_CPP.md):
on each branch of the tree, the finest cell that can be disseminated,
plus its correspondence with the finest-level cells.

- [`determiner_car_naturel()`](https://inseefrlab.github.io/gridy/reference/determiner_car_naturel.md)
  : determine the natural grid
- [`determiner_arb_naturel()`](https://inseefrlab.github.io/gridy/reference/determiner_arb_naturel.md)
  : compute the natural tree

## Estimating protected cells

Cells that cannot be disseminated on their own are estimated by
distributing the total of their group in proportion to a non-sensitive
variable.

- [`imputer_cle_repartition()`](https://inseefrlab.github.io/gridy/reference/imputer_cle_repartition.md)
  : Estimate protected mesh

## Mapping

Turning Inspire cell identifiers into geometries.

- [`make_contour_car()`](https://inseefrlab.github.io/gridy/reference/make_contour_car.md)
  : Creates a polygon from tile's ID
