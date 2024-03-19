library(sf)
library(tmap)
library(ggplot2)
library(spNetwork)
library(future) # package utilisé pour accélérer les calculs dans spNetwork
future::plan(future::multisession(workers = 5))
## Importation des couches géographiques
routes <- st_read('data/chap01/shp/Segments_de_rue.shp', quiet = TRUE)
collisions <- st_read('data/chap04/DataAccidentsSherb.shp', quiet = TRUE)
## Application de la même projection
routes <- st_transform(routes, 2949)
collisions <- st_transform(collisions, 2949)
routes <- sf::st_cast(routes, 'LINESTRING')


eval_bandwidth <- bw_cv_likelihood_calc.mc(
  bws = seq(100,1200,50),
  lines = routes,
  events = collisions,
  w = rep(1, nrow(collisions)), # le poids de chaque évènement est 1
  kernel_name = 'quartic',
  method = 'discontinuous',
  adaptive = FALSE,
  max_depth = 10,
  digits = 1,
  tol = 0.1,
  agg = 5, # les accidents dans un rayon de 5 mètres seront agrégés
  grid_shape = c(5,5),
  verbose = TRUE)


## Création des lixels d'une longueur de 100 mètres
lixels <- lixelize_lines(routes, 100, mindist = 50)
## Centroïdes des lixels
lixels_centers <- spNetwork::lines_center(lixels)


## Calcul du NKDE
intensity <- nkde.mc(lines = routes,
                     events = collisions,
                     w = rep(1, nrow(collisions)),
                     samples = lixels_centers,
                     kernel_name = 'quartic',
                     bw = 900,
                     adaptive = FALSE,
                     method = 'continuous',
                     max_depth = 8,
                     digits = 1,
                     tol = 0.1,
                     agg = 5,
                     verbose = FALSE,
                     grid_shape = c(5,5))


eval_bandwidth_adapt <- bw_cv_likelihood_calc.mc(
  bws = seq(100,1200,50),
  lines = routes,
  events = collisions,
  w = rep(1, nrow(collisions)), # le poids de chaque évènement sera 1
  kernel_name = 'quartic',
  method = 'discontinuous',
  adaptive = TRUE,
  trim_bws = seq(100,1200,50) * 2,
  max_depth = 10,
  digits = 1,
  tol = 0.1,
  agg = 5, # tous les accidents dans un rayon de 5m seront aggrégés
  grid_shape = c(5,5),
  verbose = TRUE
)

intensity_adpt <- nkde.mc(lines = routes,
                          events = collisions,
                          w = rep(1, nrow(collisions)),
                          samples = lixels_centers,
                          kernel_name = 'quartic',
                          bw = 900,
                          adaptive = TRUE,
                          trim_bw = 1800,
                          method = 'continuous',
                          max_depth = 8,
                          digits = 1,
                          tol = 0.1,
                          agg = 5,
                          verbose = TRUE,
                          grid_shape = c(5,5))


knn_net <- spNetwork::network_knn(collisions,
                                  lines = routes,
                                  k = 30,
                                  maxdistance = 3000,
                                  grid_shape = c(1,1),
                                  verbose = FALSE)
dist_mat <- knn_net$distances
# nous allons limiter les bandwidths avec des bornes de 100 à 3000m
dist_mat <- ifelse(dist_mat > 3000, 3000, dist_mat)
dist_mat <- ifelse(dist_mat < 100, 100, dist_mat)

eval_bandwidth_knearest <- bw_cv_likelihood_calc(
  bws = NULL,
  mat_bws = dist_mat,
  lines = routes,
  events = collisions,
  w = rep(1, nrow(collisions)), # le poids de chaque évènement sera 1
  kernel_name = 'quartic',
  method = 'discontinuous',
  adaptive = TRUE,
  trim_bws = NULL,
  max_depth = 10,
  digits = 1,
  tol = 0.1,
  agg = 5, # tous les accidents dans un rayon de 5 mètres seront agrégés
  grid_shape = c(5,5),
  verbose = TRUE
)


intensity_adpt_knn <- nkde.mc(lines = routes,
                              events = collisions,
                              w = rep(1, nrow(collisions)),
                              samples = lixels_centers,
                              kernel_name = 'quartic',
                              bw = dist_mat[,16],
                              trim_bw = 1800,
                              method = 'continuous',
                              max_depth = 8,
                              digits = 1,
                              tol = 0.1,
                              agg = 5,
                              verbose = TRUE,
                              grid_shape = c(5,5))

lixels$density_adpt_knn <- intensity_adpt_knn * 1000

save(intensity, intensity_adpt, intensity_adpt_knn, eval_bandwidth,
     eval_bandwidth_adapt, eval_bandwidth_knearest,
     lixels, lixels_centers,
     file = 'data/chap06/pre_calculated_results.rda')



library(sf)
library(spNetwork)
library(lubridate)
library(metR)
library(future) # utilisé pour accelérer le calcul dans spNetwork
future::plan(future::multisession(workers = 5))

routes <- st_read('data/chap01/shp/Segments_de_rue.shp')
collisions <- st_read('data/chap04/DataAccidentsSherb.shp')

# reprojection dans le même système
routes <- st_transform(routes, 32188)
collisions <- st_transform(collisions, 32188)

routes <- sf::st_cast(routes, 'LINESTRING')
routes$length <- st_length(routes)


# preparation des routes et des lixels
routes <- sf::st_cast(routes, 'LINESTRING')


library(igraph)
library(dbscan)

routes <- sf::st_cast(routes, 'LINESTRING')
routes$length <- st_length(routes)
graph <- spNetwork::build_graph(routes, digits = 2,line_weight = "length")
parts <- components(graph$graph)
graph$spvertices$part <- as.character(parts$membership)

tm_shape(graph$spvertices) +
  tm_dots("part", size = 0.1)


main_component <- subset(graph$spvertices, graph$spvertices$part == "1")

main_network <- subset(graph$spedges,
                       (graph$spedges$start_oid %in% main_component$id) |
                         (graph$spedges$end_oid %in% main_component$id)
)
main_network <- subset(main_network, as.numeric(st_length(main_network)) > 0)


lixels_main <- lixelize_lines(main_network, 100, mindist = 50)
lixels_main_centers <- spNetwork::lines_center(lixels_main)

# préparation de la colonne avec les dates
collisions$dt <- as_date(collisions$DATEINCIDE)
collisions$dt_num <- as.numeric(collisions$dt - min(collisions$dt))

# calcul des scores pour le bandwidths
cv_scores_tnkde <- bw_tnkde_cv_likelihood_calc(
  bws_net = seq(700,1500,100),
  bws_time =  seq(10,40,5),
  lines = main_network,
  events = collisions,
  time_field = "dt_num",
  w = rep(1, nrow(collisions)),
  kernel_name = "quartic",
  method = "continuous",
  max_depth = 10,
  digits = 2,
  tol = 0.1,
  agg = 10,
  grid_shape = c(5,5),
  verbose = TRUE)

# on créé un graphique pour voir les résultats
df2 <- reshape2::melt(cv_scores_tnkde)

ggplot(df2) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_contour(aes(x = Var1, y = Var2, z = value),
               breaks = c(-400,-300, -250, -200, -180, -150),
               color = 'white', linetype = 'dashed')+
  scale_fill_viridis_c() +
  labs(x = "network bandwidth (m)", y = "temporal bandwidth (days)", fill = "cv score") +
  coord_fixed(ratio=30)


# on choisit notre résolution temporelle (5 jours ici)
sample_time <- seq(0, max(collisions$dt_num), 10)

# on calcule les densités
tnkde_densities <- tnkde.mc(lines = main_network,
                            events = collisions,
                            time_field = "dt_num",
                            w = rep(1, nrow(collisions)),
                            samples_loc = lixels_main_centers,
                            samples_time = sample_time,
                            kernel_name = "quartic",
                            bw_net = 1500, bw_time = 30,
                            adaptive = TRUE,
                            trim_bw_net = 1800,
                            trim_bw_time = 60,
                            method = "continuous",
                            div = "bw", max_depth = 10,
                            digits = 2, tol = 0.01,
                            adaptive_separate = FALSE,
                            agg = 10, grid_shape = c(5,5),
                            verbose  = TRUE)


library(classInt)
library(viridis)

all_times <- min(collisions$dt) + days(sample_time)

tnkde_densities$k <- tnkde_densities$k*10000
tnkde_densities$k <- ifelse(tnkde_densities$k < 0, 0, tnkde_densities$k)

color_breaks <- classIntervals(c(tnkde_densities$k), n = 10, style = "kmeans")

all_maps <- lapply(1:length(all_times), function(i){

  dens <- tnkde_densities$k[,i]
  dt <- all_times[[i]]
  lixels_main$dens <- dens
  lixels2 <- lixels_main[order(-1*lixels_main$dens),]
  map <- tm_shape(lixels2) +
    tm_lines("dens", breaks = color_breaks$brks,
             palette = mako(10,direction = -1), lwd = 2) +
    tm_layout(frame = FALSE, legend.show=FALSE,
              main.title = as.character(all_times[[i]]))
  return(map)
})

# Création d'une animation pour produire la carte animée
tmap_animation(all_maps, filename = "images/Chap06/animated_TNKDE_sherbrooke.gif",
               width = 1000, height = 1000, dpi = 150, delay = 50)


future::plan(future::multisession(workers = 5))

routes <- st_read('data/chap01/shp/Segments_de_rue.shp', quiet = TRUE)
routes <- st_transform(routes, 2949)
routes <- sf::st_cast(routes, 'LINESTRING')

net_distances <- spNetwork::network_listw(
  origins = lixels_centers,
  lines = routes,
  maxdistance = 2500,
  mindist = 1,
  dist_func = 'identity',
  matrice_type = 'I',
  grid_shape = c(1,1),
  verbose = TRUE,
)


all_matrices <- list()

inv_weights <- lapply(net_distances$weights, function(x){
  inv <- (1/x)
  return( inv / sum(inv) )
})

net_distances_temp <- net_distances
net_distances_temp$weights <- inv_weights
all_matrices[["inv_mat"]] <- net_distances_temp

inv2_weights <- lapply(net_distances$weights, function(x){
  inv <- (1/x**2)
  return( inv / sum(inv) )
})

net_distances_temp <- net_distances
net_distances_temp$weights <- inv2_weights
all_matrices[["inv2_mat"]] <- net_distances_temp

bin_dists <- c(250,300,500,700,900)

for(d in bin_dists){
  new_weights <- lapply(net_distances$weights, function(x){
    if(is.null(x) == FALSE){
      w <- spNetwork::quartic_kernel(x,d)
      return( (w) / sum(w))
    }
    else{
      return(NULL)
    }
  })
  net_distances_temp <- net_distances
  net_distances_temp$weights <- new_weights
  all_matrices[[paste0("dist_quartic_",d)]] <- net_distances_temp
}


for(k in 3:10){

  new_weights <- lapply(net_distances$weights, function(x){
    x_r <- rank(x, ties.method = "min")
    return( (x_r <= k) / sum(x_r <= k))
  })
  net_distances_temp <- net_distances
  net_distances_temp$weights <- new_weights
  all_matrices[[paste0("k_mat_",k)]] <- net_distances_temp

}

bin_dists <- c(250,300,500,700,900)

for(d in bin_dists){
  new_weights <- lapply(net_distances$weights, function(x){
    return( (x <= d) / sum(x <= d))
  })
  net_distances_temp <- net_distances
  net_distances_temp$weights <- new_weights
  all_matrices[[paste0("dist_mat_",d)]] <- net_distances_temp
}


library(spdep)

moran_vals <- sapply(all_matrices, function(W){

  #petite convertion vers le type de spdep
  attr(W,'class') <- c("listw", "nb")
  W$style <- "W"
  W$neighbours <- W$nb_list
  W$nb_list <- NULL

  val <- moran(lixels$density_adpt_knn, listw = W,
               n = nrow(lixels),
               S0 = nrow(lixels),
               zero.policy = TRUE)
  return(val$I)
})

df_moran <- data.frame(
  Matrices = names(all_matrices),
  MoranIs = moran_vals
)

ggplot(data=df_moran, aes(x=reorder(Matrices,MoranIs), y=MoranIs)) +
  geom_segment( aes(x=reorder(Matrices,MoranIs),
                    xend=reorder(Matrices,MoranIs),
                    y=0, yend=MoranIs)) +
  geom_point( size=4,fill="red",shape=21)+
  xlab("Matrice de pondération spatiale sur réseau") +
  ylab("I de Moran")+
  coord_flip()


save(lixels_main, lixels_main_centers, sample_time, cv_scores_tnkde, tnkde_densities,
     file = 'data/chap06/pre_calculated_results_tnkdeA.rda')

save(net_distances,
     file = 'data/chap06/pre_calculated_results_tnkdeB.rda')

#tools::resaveRdaFiles(c('data/chap06/pre_calculated_results_tnkde.rda'))
tools::resaveRdaFiles(c('data/chap06/pre_calculated_results_tnkdeA.rda'))
tools::resaveRdaFiles(c('data/chap06/pre_calculated_results_tnkdeB.rda'))
tools::resaveRdaFiles(c('data/chap06/pre_calculated_results.rda'))

