#' Creates a polygon from tile's ID
#'
#' @param id_car id of tile, in INSPIRE style
#'
#' @return
#'
#' @examples
#' make_contour_car("CRS3035RES200mN2009400E2992400")
#'
#' @export
make_contour_car <- function(id_car){
  id_car <- as.character(id_car)
  a <- strsplit(id_car,"RES")[[1]]
  a <- strsplit(a[2], "mN")[[1]]

  #la taille de la maille
  maille <- as.numeric(a[1])

  #les coordonnées du coin en bas à gauche
  coord <- as.numeric(strsplit(a[2], "E")[[1]])
  x <- coord[2]
  y <- coord[1]

  #créer le polygone
  mp <- matrix(c(x, y,
                 x, y + maille,
                 x + maille, y + maille,
                 x + maille, y,
                 x, y), ncol = 2, byrow = TRUE)

  pol <- sf::st_polygon(list(mp), dim = "XY")
  return(pol)
}

#Fonction permettant de passer la grille de carreau en objet sf
#vers un objet de type raster:
to_raster <- function(t_geo, crs, taille, nom_var = NULL){
  if(is.null(nom_var)){
    nom_var <- colnames(t_geo)
    nom_var <- nom_var[nom_var != "geometry"]
  }
  l <- length(nom_var)

  r <- vector("list", l)
  r0 <- fasterize::raster(t_geo, crs = crs, resolution = taille)
  for(i in 1:l){
    r[[i]] <- fasterize::fasterize(t_geo, r0, field = nom_var[i])
    names(r[[i]]) <- nom_var[i]
  }

  rb <- do.call(raster::brick, r)

  return(rb)
}
