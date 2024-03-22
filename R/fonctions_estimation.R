.datatable.aware <- TRUE

#' Estimate protected mesh
#'
#' The information on some mesh cannot
#' be disseminated because it doesn't respect the threshold rule.
#' For those mesh, an estimation is done by distributing the total
#' of the variable on a bigger zone (the groupe) into the meshes,
#' proportionnaly to a given variable (non sensitive variable).
#'
#' @param tab_car A data.table of the variables agregates in a grid.
#' @param list_var_imput A character vector giving the names of the variables
#' to estimate.
#' @param var_cle A character, the name of the non-sensitive variable
#' according to which the distribution is done.
#'
#' @return The data.table \code{tab_car} with the estimated variables.
#'
#' @examples
#' library(data.table)
#' n <- 1e4
#' tab <- as.data.table(
#'   data.frame(id_obs = 1:n, x = rnorm(n, 3e6, 2e4),
#'   y = rnorm(n, 2e6, 3e4), crs = 3035))
#' tab_GS <- create_GS_CPP(tab, 5, c(32e3,16e3,8e3,4e3,2e3,1e3))
#' tab_car <- tab_GS$tab_car
#' tab_car[, `:=`(poph = nb_obs*0.48, popf = nb_obs*0.52)]
#' tab_diff <- imputer_cle_repartition(
#'   tab_car,
#'   list_var_imput = c("poph","popf"),
#'   var_cle = "nb_obs")
#'
#' @export
imputer_cle_repartition <- function(tab_car, list_var_imput, var_cle){

  cle = groupe = id_carreau_nat = NULL
  # due to NSE notes in R CMD check

  list_var_imput_2 = paste0(list_var_imput,"_diffusion")
  list_var_imput_3 = paste0(list_var_imput,"_tot_gpe")

  tab_car[, cle := get(var_cle)/sum(get(var_cle)), by=.(groupe)]

  tab_car[, (list_var_imput_3) := lapply(list_var_imput, function(x){sum(get(x))}), by=.(groupe)]

  tab_car[, (list_var_imput_2) := lapply(list_var_imput_3, function(x){get(x)*cle})]

  return(tab_car)
}
