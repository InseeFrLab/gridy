#Fonctions pour creer les grilles
#https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html
.datatable.aware <- TRUE

#' Create a square grid
#'
#' @param tab A data.table with at least a column for x coordinate,
#' a column for y coordinate and a column specifying the coordinate
#' reference system (crs). For exemple, crs = 3035 for the LAEA projection
#' in Europe.
#' @param taille The size of the mesh in meters.
#' @param nom_id_car A character, the name of the variable for the
#' identifier of the mesh. By default it is "id_carreau".
#' @param point_base vector of 2 numeric values, coordinates of the reference point of
#' the grid. If eurostat = TRUE, the value used will be automatically c(0,0).
#' @param eurostat boolean, whether the id created has to comply the eurostat
#' inspire requirements or not, especially for the conversion in km while using
#' a resolution >= 1000m.
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
#' library(data.table)
#' tab <- as.data.table(data.frame(id_obs = 1:10, x = rnorm(10,3e6,1e4), y = rnorm(10, 2e6, 1e4),
#' crs = 3035))
#' res <- create_grid_niv(tab, 200)
#' @export
create_grid_niv <- function(
    tab, taille,
    nom_id_car = "id_carreau",
    point_base = c(0, 0),
    eurostat = FALSE
){
  # Init objet résultat

  resul <- data.table::copy(as.data.table(tab))

  #point de base modulo la taille:
  point_base <- point_base %% taille

  #Avec la norme Inspire pour les grilles en Europe:
  if(eurostat){

    n <- floor(log10(taille))
    #pour réduire la taille de la chaîne de caractère dans la norme Inspire
    while(floor(taille/(10^n)) != taille/(10^n)) n <- n-1

    if(taille < 1000)
      nom_taille <- paste0(taille,"m")
    else
      nom_taille <- paste0(floor(taille/1e3),"km")

    resul[,
          (nom_id_car) :=
            paste0(
              "FR_CRS",crs,"RES", nom_taille,
              "N",floor(y / taille) * taille / (10^n),
              "E",floor(x / taille) * taille / (10^n)
            )
    ]
  }else{
    #L'autre norme Inspire
    resul[,
          (nom_id_car) :=
            paste0(
              "FR_CRS",crs,"RES",taille,"m",
              "N", as.integer(floor((y - point_base[2]) / taille) * taille + point_base[2]),
              "E", as.integer(floor((x - point_base[1]) / taille) * taille + point_base[1])
            )
    ]
  }

  return(resul)
}


#' Create several square grids
#'
#' @inheritParams create_grid_niv
#' @param tab A data.table with at least a column for x coordinate,
#' a column for y coordinate and a column specifying the coordinate
#' reference system (crs). For exemple, crs = 3035 for the LAEA projection
#' in Europe.
#' @param mailles A vector of numbers indicating the sizes of the mesh
#' of the different grid in a decreasing order.If mailles has names, they will
#' be used to create the name of the variable.
#'
#' @return The data.table \code{tab} with one more column for each grid.
#' The names of these column are "id_carreau_nivX" where X stands for
#' the level of the grid in decreasing order (level 1 for the bigger
#' grid).
#'
#' @examples
#' library(data.table)
#' tab <- as.data.table(data.frame(id_obs = 1:10, x = rnorm(10,3e6,1e4), y = rnorm(10, 2e6, 1e4),
#' crs = 3035))
#' grids1 <- create_grids(tab, c(1000,200))
#' grids2 <- create_grids(tab, c("1km" = 1000, "200m" = 200))
#'
#' @importFrom rlang .data
#'
#' @export
create_grids <- function(tab, mailles, eurostat = FALSE){

  resul <- data.table::copy(tab)
  mailles <- rev(mailles[order(mailles)])

  purrr::iwalk(
    mailles,
    function(x,i){
      var = paste0("id_carreau_niv_", i)
      e_par <- rlang::env_parent()
      e_par$resul <- create_grid_niv(e_par$resul, x, nom_id_car = var, eurostat = eurostat)
    }
  )

  return(resul)
}

#Fonction pour determiner la commune principale a laquelle appartient le carreau
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

#Fonction pour determiner les communes reconstituables entierement
# a partir de carreaux
comp_connexe_carcom <- function(tab, var1, var2){
  t_ind <- data.table::copy(tab)
  setnames(t_ind,c(var1, var2), c("z1", "z2")) #pour etre coherent avec les notations
  #des fonctions du package diffman

  t_crois <- diffman::tab_crois(t_ind)
  t_crois <- diffman::simplify_z2_fus(t_crois)
  m_crois <- diffman::matrix_crois(t_crois)
  m_liens <- diffman::matrix_liens(m_crois)

  ind_com_isolees <- which(apply(m_liens,1, sum) == 0)
  com_isolees <- rownames(m_liens)[ind_com_isolees]

  return(com_isolees)
}


