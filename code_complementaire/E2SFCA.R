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
  Matrice$W  <-ifelse(Matrice[[CoutDistance]] < Palier, 1,
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

