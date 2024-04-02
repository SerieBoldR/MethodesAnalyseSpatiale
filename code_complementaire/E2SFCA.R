## Fonction pour la méthode E2SFCA avec une fonction de gradient continue
#' @param MatriceOD matrice de distance OD.
#' @param IDorigine Champ identifiant pour l'origine dans la matrice OD.
#' @param IDdestination Champ identifiant pour la destination dans la matrice OD.
#' @param CoutDistance Coût pour la distance dans la matrice OD.
#' @param RatioHabitants Ratio pour le nombre d'habitants (1000 par défaut).
#' @param Wo pondération pour la demande (population à l'origine).
#' @param Wd pondération pour l'offre (taille du service à la destination).
GE2SFCA <- function(MatriceOD, IDorigine, IDdestination, CoutDistance,
                    RatioHabitants = 1000, Rayon, Palier, Wo, Wd, ChampSortie){
  ## Suppression des observations avec un coût supérieure au rayon
  Matrice <- subset(MatriceOD, MatriceOD[[CoutDistance]] <= Rayon)
  ## Calcul de Pk, soit ((60 - CoutDistance)/(60 - 10))^1.5;
  Matrice$W  <- ifelse(Matrice[[CoutDistance]] < Palier, 1,
                      ((Rayon - Matrice[[CoutDistance]]) / (Rayon - Palier))**1.5)
  Matrice$Pk <- Matrice$W * Matrice[[Wo]]
  ## Étape 1 : Ratio Population/service dans le rayon autour des destinations
  Step1 <- cbind(aggregate(Matrice[[Wd]] ~ Matrice[[IDdestination]], FUN = min),
                 aggregate(Matrice$Pk ~ Matrice[[IDdestination]], FUN = sum)
  )
  Step1 <- Step1[, c(1,2,4)]
  names(Step1) <- c(IDdestination, Wd, Wo)
  Step1$Rj <- Step1[[Wd]] / (Step1[[Wo]] / RatioHabitants)
  Step1 <- Step1[, c(1,4)]
  
  ## Étape 2 : ramener la somme de ces ratios pour les origines
  Jointure <- merge(Matrice, Step1, by = IDdestination, all.x=FALSE, all.y=FALSE)
  Jointure <- as.data.frame(Jointure)
  gD2SFCA  <- aggregate(Jointure$Rj ~ Jointure[[IDorigine]], FUN = sum)
  names(gD2SFCA) <- c(IDorigine, ChampSortie)
  return(gD2SFCA)
}



## Fonction pour la méthode 2SFCA acceptant n'importe quelle fonction pour la pondération
#' @param dist_mat matrice de distance OD
#' @param Wfun la fonction de pondération convertissant une durée en pondération de 0 à 1
#' @param IDorigine le nom de la colonne dans dist_mat servant d'identifiant unique des origines
#' @param IDdestination le nom de la colonne dans dist_mat servant d'identifiant unique des destinations
#' @param CoutDistance le nom de la colonne dans dist_mat comprenant les temps estimés entre origines et destinations
#' @param Wo le nom de la colonne dans dist_mat indiquant pondération pour la demande (population à l'origine).
#' @param Wd le nom de la colonne dans dist_mat indiquant pondération pour l'offre (taille du service à la destination).
#' @param ChampSortie le nom que devra avoir la colonne avec les valeur de 2SFCA dans le dataframe de sortie
GTSFCA <- function(dist_mat, Wfun, IDorigine, IDdestination, CoutDistance,Wo, Wd, ChampSortie){
  

  dist_mat$Wdist <- Wfun(dist_mat[[CoutDistance]])
  dist_mat$Wo <- dist_mat[[Wo]]
  dist_mat$Wd <- dist_mat[[Wd]]
  dist_mat$from_id <- dist_mat[[IDorigine]]
  dist_mat$to_id <- dist_mat[[IDdestination]]
  
  # on commence par calculer les Rj, soit la somme des 
  # des totaux de population multipliés par le poids
  # associé à la distance
  R <- dist_mat %>% 
    group_by(to_id) %>% 
    summarise(denom = sum(Wdist * Wo),
              num = first(Wd)
    ) %>% 
    mutate(rj = num / denom)
  
  # on calcule ensuite les Ai, soit la somme des RJ pondérés par la distance
  dist_mat$rj <- R$rj[match(dist_mat$to_id, R$to_id)]
  
  A <- dist_mat %>% 
    group_by(from_id) %>% 
    summarise(ai = sum(Wdist * rj))
  names(A) <- c(IDorigine, ChampSortie)
  
  return(A)
}

sig_fun <- function(x,a,b){
  (1 + exp(-(a*pi)/(b*sqrt(3)))) / (1 + exp(((x-a)*pi)/(b*sqrt(3))))
}


# # comparaison des deux fonctions
# load("data/chap05/Resultats/matriceODPatinoire.Rdata")
# 
# Ilots <- st_read(dsn = "data/chap05/AutresDonnees/Commerces.gpkg",
#                  layer = "Ilots", quiet=TRUE)
# 
# TempIlots <- st_drop_geometry(Ilots)
# 
# matriceODPatinoire <- merge(matriceODPatinoire.Marche, 
#                             TempIlots[,c("id","pop2021")],
#                             by.x = "from_id", by.y="id")
# 
# matriceODPatinoire$Wd <- 1
# names(matriceODPatinoire) <- c("from_id", "to_id", "Marche", "Wo", "Wd")
# head(matriceODPatinoire, n=2)
# 
# MesureE2SFCA <- GE2SFCA(MatriceOD = matriceODPatinoire,
#                         IDorigine = "from_id",
#                         IDdestination = "to_id",
#                         CoutDistance = "Marche",
#                         RatioHabitants = 1000,
#                         Rayon = 30,
#                         Palier = 5,
#                         Wo = "Wo",
#                         Wd = "Wd",
#                         ChampSortie = "E2SFCA_G")
# 
# # pour utiliser la seconde fonction, nous devons déterminer une fonction 
# # de pondération. 
# Wfun <- function(x){
#   w <- ifelse(x < 5, 1,((30 - x) / (30 - 5))**1.5)
#   w[x  > 30] <- 0
#   return(w)
# }
# 
# matriceODPatinoire$Wo <- matriceODPatinoire$Wo / 1000
# 
# MesureE2SFCA_B <- GTSFCA(dist_mat = matriceODPatinoire,
#                          Wfun = Wfun,
#                          IDorigine = "from_id",
#                          IDdestination = "to_id",
#                          CoutDistance = "Marche",
#                          Wo = "Wo",
#                          Wd = "Wd",
#                          ChampSortie = "E2SFCA_GB")
# 
# 
# test <- merge(MesureE2SFCA, MesureE2SFCA_B, by = 'from_id')
