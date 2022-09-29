// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List det_etat(IntegerVector nb, bool ep, int fp, int gp, int seuil, int max_gpe) {
  
  int Tot_B = 0;
  int n = nb.size();
  int gpe_actu = max_gpe + 1;
  
  LogicalVector etat(n);
  NumericVector force(n);
  NumericVector groupe(n);
  
  if(ep){
    for(int i = 0; i<n; ++i){
      if(nb[i] < seuil){
        etat[i] = FALSE ;
        force[i] = 0;
        groupe[i] = gpe_actu;
        Tot_B += nb[i];
      }
      else if((0 < Tot_B) & (Tot_B < seuil)){
        etat[i] = FALSE;
        force[i] = seuil - Tot_B;
        groupe[i] = gpe_actu;
        Tot_B += nb[i];
      }
      else{
        etat[i] = TRUE;
        force[i] = 0;
        ++ gpe_actu;
        groupe[i] = gpe_actu;
      }
    }
  }
  
  else if(fp == 0){
    for(int i = 0 ; i<n ; ++i){
      etat[i] = FALSE;
      force[i] = 0;
      groupe[i] = gp;
    }
  }
  
  else{
    for(int i = 0; i<n; ++i){
      if(nb[i] < seuil){
        etat[i] = FALSE ;
        force[i] = 0;
        groupe[i] = gp;
        Tot_B += nb[i];
      }
      else if(Tot_B < fp){
        etat[i] = FALSE;
        force[i] = fp - Tot_B;
        groupe[i] = gp;
        Tot_B += nb[i];
      }
      else{
        etat[i] = TRUE;
        force[i] = 0;
        groupe[i] = gpe_actu;
        ++ gpe_actu;
      }
    }
  }
  
  List ret;
  ret["etat"] = etat;
  ret["force"] = force;
  ret["groupe"] = groupe;
  return(ret);
}

// [[Rcpp::export]]
List det_etat_tot(CharacterVector id_car_pere,
                  LogicalVector etats_pere,
                  IntegerVector forces_pere,
                  IntegerVector groupes_pere,
                  CharacterVector id_car_pere_fils,
                  IntegerVector nb_obs_fils,
                  int seuil){
  
  int n = id_car_pere.size();
  int m = nb_obs_fils.size();
  int max_gpe = max(groupes_pere);
  
  LogicalVector etat_fils(m);
  IntegerVector force_fils(m);
  IntegerVector groupe_fils(m);
  
  int barWidth = 100;
  
  for(int j = 0; j<n; j++){
    String id_p = id_car_pere[j];
    
    int i1 = 0;
    int i2 = 0;
    while((i1 < m) & (id_car_pere_fils[i1] != id_p)){i1++ ; i2++;}
    while((i2 < m) & (id_car_pere_fils[i2] == id_p)){i2++;}
    
    IntegerVector nb(i2-i1);
    bool ep = etats_pere[j];
    int fp = forces_pere[j];
    int gp = groupes_pere[j];
    for(int i = i1; i<i2; i++){
      nb[i-i1] = nb_obs_fils[i];
    }
    
    List lres = det_etat(nb, ep, fp, gp, seuil, max_gpe);
    LogicalVector l_etat = lres["etat"];
    IntegerVector l_force = lres["force"];
    IntegerVector l_groupe = lres["groupe"];
    
    for(int i = i1; i<i2; i++){
      etat_fils[i] = l_etat[i-i1];
      force_fils[i] = l_force[i-i1];
      groupe_fils[i] = l_groupe[i-i1];
    }
    
    int maxl = max(l_groupe);
    if(maxl > max_gpe) max_gpe = maxl;
    
    if(n > 5000){
    if((j % 5000 == 0)){
      double jf = j;
      float progress = jf/n;
      std::cout << "[";
      int pos = barWidth * progress;
      for (int i = 0; i < barWidth; ++i) {
        if (i < pos) std::cout << "=";
        else if (i == pos) std::cout << ">";
        else std::cout << " ";
      }
      std::cout << "] " << int(progress * 100.0) << " %\r";
      std::cout.flush();
    }}
  }
  
  std::cout << std::endl;
  
  List ret;
  ret["etat"] = etat_fils;
  ret["force"] = force_fils;
  ret["groupe"] = groupe_fils;
  return(ret);
  
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// # /*** R
// # #det_etat(c(1,4,12,15,23), TRUE, 0, 34, 11, 100)
// #   */
