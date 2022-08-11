#Fonctions pour créerles grilles


#' Create a square grid
#'
#' @param tab A data.table with at least a column for x coordinate,
#' a column for y coordinate and a column specifying the coordinate
#' reference system (crs). For exemple, crs = 3035 for the LAEA projection
#' in Europe.
#' @param taille The size of the mesh in meters.
#' @param nom_id_car A character, the name of the variable for the
#' identifier of the mesh. By default it is "id_carreau".
#'
#' @return The data.table \code{tab} with one more column beeing
#' the identifier of the mesh of the square grid. The name of the
#' mesh is based on the Inspire norm :
#' "if the coordinate reference system is projected, the word RES followed
#' by the grid resolution in meters and the letter m. Then, the letter N
#' followed by the northing value in meters, and the letter E followed by
#' the easting value in meters too" (Inspire, Data Specification for the
#' spatial data theme Statistical Units, 10/12/2013, page 30). The given
#' position is the position of the lower left cell corner.
#'
#' @examples
#' tab <- as.data.table(data.frame(id_obs = 1:10, x = rnorm(10,3e6,1e4), y = rnorm(10, 2e6, 1e4),
#' crs = 3035))
#' tab <- create_grid_niv(tab, 200)
create_grid_niv <- function(tab, taille, nom_id_car = "id_carreau", point_base = c(0, 0)){
  # Init objet résultat
  resul <- tab

  #point de base modulo la taille:
  point_base <- point_base %% taille

  #Avec la norme Inspire pour les grilles en Europe:
  # n <- floor(log10(taille)) #pour réduire la taille de la chaîne de caractère dans la norme Inspire
  # while(floor(taille/(10^n)) != taille/(10^n)) n <- n-1
  #
  # if(taille < 1000)
  #   nom_taille <- paste0(taille,"m")
  # else
  #   nom_taille <- paste0(floor(taille/1e3),"km")
  # resul[, (nom_var) := paste0(nom_taille,
  #                             "N",floor(y / taille) * taille / (10^n),
  #                             "E",floor(x / taille) * taille / (10^n)) ]


  #L'autre norme Inspire
  resul[, (nom_id_car) := paste0("CRS",crs,"RES",taille,"m",
                              "N", as.integer(floor((y - point_base[2]) / taille) * taille + point_base[2]),
                              "E", as.integer(floor((x - point_base[1]) / taille) * taille + point_base[1]))]


  return(resul)
}


#' Create several square grids
#'
#' @param tab A data.table with at least a column for x coordinate,
#' a column for y coordinate and a column specifying the coordinate
#' reference system (crs). For exemple, crs = 3035 for the LAEA projection
#' in Europe.
#' @param mailles A vector of numbers indicating the sizes of the mesh
#' of the different grid in a decreasing order.
#'
#' @return The data.table \code{tab} with one more column for each grid.
#' The names of these column are "id_carreau_nivX" where X stands for
#' the level of the grid in decreasing order (level 1 for the bigger
#' grid).
#'
#' @examples
#' tab <- as.data.table(data.frame(id_obs = 1:10, x = rnorm(10,3e6,1e4), y = rnorm(10, 2e6, 1e4),
#' crs = 3035))
#' tab <- create_grids(tab, c(1000,200))
create_grids <- function(tab, mailles, nom_id_car_petit = NULL){
  resul <- tab
  mailles <- rev(mailles[order(mailles)]) #pour classer en ordre décroissant les tailles de carreaux
  for(i in 1:length(mailles)){
    nom_var <- paste0("id_carreau_niv", i)
    if(!is.null(nom_id_car_petit) & i == length(mailles)) nom_var <- nom_id_car_petit
    resul <- create_grid_niv(resul, mailles[i], nom_var)
  }

  return(resul)
}

#Fonction pour d?terminer la commune principale ? laquelle appartient le carreau
main_depcom_on_mesh <- function(tab, taille){
  f <- function(a, num){
    ta <- table(a)
    na <- names(ta[order(ta, decreasing = TRUE)])
    return(na[num])
  }


  tab <- create_grid_niv(tab, taille)
  tab_car_com <- tab[, .(depcom_1 = f(depcom,1), depcom_2 = f(depcom,2)), by = .(id_carreau)]

  return(tab_car_com)
}

#Fonction pour d?terminer les communes reconstituables enti?rement
# ? partir de carreaux
comp_connexe_carcom <- function(tab, var1, var2){
  t_ind <- copy(tab)
  setnames(t_ind,c(var1, var2), c("z1", "z2")) #pour ?tre coh?rent avec les notations
  #des fonctions du package diffman

  t_crois <- diffman::tab_crois(t_ind)
  t_crois <- diffman::simplify_z2_fus(t_crois)
  m_crois <- diffman::matrix_crois(t_crois)
  m_liens <- diffman::matrix_liens(m_crois)

  ind_com_isolees <- which(apply(m_liens,1, sum) == 0)
  com_isolees <- rownames(m_liens)[ind_com_isolees]

  return(com_isolees)
}


