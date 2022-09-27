#devtools::find_rtools(FALSE, TRUE)
#sourceCpp("X:/HAB-Cartographie/Carroyage/Grilles_superposees/Version3_R/gridy/Rcpp/determiner_etat_carreaux.cpp")



#' Create confidential grids
#'
#' The confidentiality rule is not to have
#' meshs containing less observations than
#' the threshold.
#'
#' @param tab A data.table with at least (x, y, crs) columns.
#' @param seuil The confidentiality threshold.
#' @param mailles A vector with the sizes of the different
#' grids.
#'
#' @return At the end of the process, we get a data.table
#' with one row for each mesh of each grid. In the columns
#' we have the identifier of the mesh, the identifier of
#' the mesh at the above level, the number of observation
#' in the mesh, the state of the mesh (can the information
#' on the mesh be disseminated without perturbation or not),
#' the force of the mesh and the group of the mesh.
#'
#' @examples
#'n <- 1e4
#'tab <- as.data.table(data.frame(id_obs = 1:n, x = rnorm(n, 3e6, 2e4), y = rnorm(n, 2e6, 3e4), crs = 3035))
#'tab_grid <- create_GS_CPP(tab, 5, c(32e3,16e3,8e3,4e3,2e3,1e3))
#' @export
create_GS_CPP <- function(tab, seuil, mailles, ...){
  niv_max <- length(mailles) #nombre de niveau et niveau maximum des GS

  print("Etape 1 : création des différentes grilles *")
  tab <- create_grid_niv(tab, mailles[niv_max], nom_id_car = "id_carreau_petit", ...)
  t_car <- tab[, .(nb_obs = .N, x = first(x), y = first(y), crs = first(crs)) , by = .(id_carreau_petit)]
  for(n in 1:(niv_max - 1))
    t_car <- create_grid_niv(t_car, taille = mailles[n], nom_id_car = paste0("id_carreau_niv",n))


  print("Etape 2 : Initialiser la table de diffusion 'tdiff' **")
  tcar_1 <- t_car[, .(p = NA, niveau = 1, nb_obs = sum(nb_obs)) , by=. (id_carreau_niv1)]
  tdiff <- tcar_1[, ":="(etat = nb_obs >= seuil, force = 0, groupe = 1:(.N))]
  names(tdiff)[names(tdiff) == "id_carreau_niv1"] <- "id_carreau"

  print("Etape 3 : On complete la table de diffusion, en parcourant chaque carreau, du plus grand au plus petit ***")
  for(i in 1:(niv_max - 1)){
    print(paste0("      Traitement des carreaux de niveau ",i))

    #On créer une clé de rangement pour la table t_car
    setkeyv(t_car, paste0("id_carreau_niv",i))

    #on sélectionne la partie de tdiff qui va être utile
    tdiff_niv_i <- tdiff[niveau == i]
    #setkey(tdiff_niv_i, id_carreau) #on créer une clé de rangement pour accélérer les subseting

    #Les carreaux du niveau i
    ids_car <- tdiff_niv_i$id_carreau

    #Table des carreaux de niveau niv_pere + 1
    nom_id_car <- paste0("id_carreau_niv",i + 1)
    if(i == niv_max -1) nom_id_car <- "id_carreau_petit"
    t_car_p <- t_car[,
                     .(p =first(get(paste0("id_carreau_niv",i))) ,niveau = i+1, nb_obs = sum(nb_obs)) ,
                     by=.(id_carreau = get(nom_id_car))]

    #Trier les tables
    t_car_p <- t_car_p[order(p,nb_obs)] #on tri par ordre croissant du nombre d'observations


    #Parcourir chaque carreau
    lres <- det_etat_tot(ids_car, tdiff_niv_i$etat, tdiff_niv_i$force, tdiff_niv_i$groupe,
                         t_car_p$p, t_car_p$nb_obs, seuil)

    tdiff_niv_plus <- as.data.table(data.frame(id_carreau = t_car_p$id_carreau,
                                               p = t_car_p$p,
                                               niveau = i+1,
                                               nb_obs = t_car_p$nb_obs,
                                               etat = lres$etat,
                                               force = lres$force,
                                               groupe = lres$groupe))

    #on complète la table de diffusion finale des carreaux
    tdiff <- rbind(tdiff, tdiff_niv_plus)


  }

  return(list(tab_arb = t_car, tab_car = tdiff))
}


determiner_car_naturel <- function(resul_GS){

  #Table indiquant l'?tat des carreaux (diffus? ou non)
  tab_car_etat <- resul_GS[[2]]
  niv_fin <- max(tab_car_etat$niveau) #niveau le plus fin des grilles superposées
  id_car_fin = paste0("id_carreau_niv",niv_fin)

  #Liens de parent?s entre les diff?rents carreaux (table d'arborescence)
  tab_arb <- resul_GS[[1]]
  setnames(tab_arb, "id_carreau_petit", id_car_fin)
  tab_arb <- tab_arb[!duplicated(get(id_car_fin)), ]

  # On ne garde que les carreaux diffus?s (etat = 1) :
  tab_car_diff <- tab_car_etat[etat == 1]
  tab_reserve <- copy(tab_car_diff)

  #On va traiter niveau par niveau :
  for(niv in 1:(niv_fin - 1)){
    tniv <- tab_reserve[niveau == niv]
    v1 <- paste0("id_carreau_niv",niv)
    v2 <- paste0("id_carreau_niv",niv+1)
    tfils <- tab_arb[,.(id_carreau = get(v1),id_fils = get(v2))]
    tfils <- tfils[!duplicated(id_fils)]
    tfils <- tfils[id_carreau %in% tniv$id_carreau]
    tfils <- merge(tfils,
                   tab_car_diff[niveau == niv + 1 ,.(id_fils = id_carreau, etat_fils = etat)],
                   by= "id_fils", all.x = TRUE)
    tfils[is.na(etat_fils), etat_fils := 0]
    tnat <-  tfils[, .(etat_nat = sum(etat_fils == 0) > 0, niveau = niv), by=.(id_carreau)]

    if(niv == 1) tnat_fin <- tnat
    else tnat_fin <- rbind(tnat_fin, tnat)

    #On retire toute la descendance des carreaux diffus?s au niveau naturel
    id_car_nat <- tnat[etat_nat == TRUE]$id_carreau
    for(i in (niv+1):niv_fin){
      vdesc <- paste0("id_carreau_niv",i)
      tdesc <- tab_arb[,.(id_carreau = get(v1),id_fils = get(vdesc))]
      tdesc <- tdesc[id_carreau %in% id_car_nat]
      tab_reserve <- tab_reserve[!(id_carreau %in% tdesc$id_fils & niveau == i)]
    }


    print(paste0("fin du niveau ", niv))
  }

  tab_car_etat <- merge(tab_car_etat, tnat_fin, by=c("id_carreau","niveau"), all.x = TRUE)
  tab_car_etat[is.na(etat_nat), etat_nat := FALSE]
  tab_car_etat[niveau == niv_fin & etat == 1 & id_carreau %in% tab_reserve[niveau == niv_fin]$id_carreau, etat_nat := TRUE]

  tab_car_nat <- tab_car_etat[etat_nat == TRUE, .(id_carreau_nat = id_carreau, niveau)]

  return(tab_car_nat)
}


determiner_arb_naturel <- function(tab_car_nat, resul_GS){
  tab_car_etat <- resul_GS[[2]]
  niv_fin <- max(tab_car_etat$niveau) #niveau le plus fin des grilles superposées
  id_car_fin = paste0("id_carreau_niv",niv_fin)

  #Liens de parent?s entre les diff?rents carreaux (table d'arborescence)
  tab_arb <- resul_GS[[1]]
  tab_arb <- tab_arb[!duplicated(get(id_car_fin)), ]

  tab_rel_nat <- NULL
  for(niv in 1:(niv_fin - 1)){
    name_id_niv <- paste0("id_carreau_niv",niv)
    col <- c(id_car_fin, name_id_niv)
    tab_temp <- tab_arb[, col, with = FALSE]
    tab_car_niv <- merge(tab_car_nat[niveau == niv], tab_temp, by.x = "id_carreau_nat",  by.y = name_id_niv, all.x = TRUE, all.y = FALSE)
    tab_rel_nat <- rbind(tab_rel_nat, tab_car_niv)
  }

  #Ajout du niveau le plus fin (niveau 7):
  tab_ajout_niv_fin <- tab_car_nat[niveau == niv_fin,][,id_carreau_fin:= id_carreau_nat]
  setnames(tab_ajout_niv_fin, "id_carreau_fin", id_car_fin)
  tab_rel_nat <- rbind(tab_rel_nat, tab_ajout_niv_fin)


  return(tab_rel_nat)
}






