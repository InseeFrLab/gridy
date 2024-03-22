test_that("Consistency of observations number", {
  library(data.table)
  n <- 1e4
  niveaux <- c(32e3,16e3,8e3,4e3,2e3,1e3)

  tab <- as.data.table(data.frame(id_obs = 1:n, x = rnorm(n, 3e6, 2e4), y = rnorm(n, 2e6, 3e4), crs = 3035))
  tab_grid <- create_GS_CPP(tab, 5, niveaux)
  tot_obs <- tab_grid$tab_car[,.(tot = sum(nb_obs)), by = niveau]
  expect_equal(tot_obs$tot, rep(nrow(tab), length(niveaux)))
})
