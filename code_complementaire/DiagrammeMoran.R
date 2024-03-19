#' Fonction pour créer le diagramme de Moran dans un contexte univarié.
#' @param x un vecteur avec la variable continue X.
#' @param listW un objet listw de spdep (matrice de pondération spatiale).
#' @param titre du graphique
#' @param titreAxeX titre de l'axe des X
#' @param titreAxeY titre de l'axe des X
#' @param AfficheAide indique l'affichage de l'aide de lecture dans le graphique
#' @return un objet ggplot2.
DiagMoranUnivarie <- function(x,listW,
                              titre="Diagramme de Moran",
                              titreAxeX = "X",
                              titreAxeY = "Variable spatialement décalée",
                              AfficheAide=FALSE)
{
  zx <- (x - mean(x))/sd(x)     # variable X centrée réduite (cote z)
  wzx <- lag.listw(listW,zx)    # variable X centrée réduite spatialement décalée

  typo <- ifelse(zx>0 & wzx>0, "HH", "ND")
  typo <- ifelse(zx<0 & wzx<0, "LL", typo)
  typo <- ifelse(zx>0 & wzx<0, "HL", typo)
  typo <- ifelse(zx<0 & wzx>0, "LH", typo)

  couleurs <- c("HH" = "darkred",
                "LL" = "darkblue",
                "HL" = "red",
                "LH" = "blue")

  if (AfficheAide){
    notetxt = "Autocorrélation positive\n"
    notetxt = paste(notetxt, "HH : valeur forte dans un contexte de valeurs fortes\n")
    notetxt = paste(notetxt, "LL : valeur faible dans un contexte de valeurs faibles\n")
    notetxt = paste(notetxt, "Autocorrélation négative\n")
    notetxt = paste(notetxt, "HL : valeur forte dans un contexte de valeurs faibles\n")
    notetxt = paste(notetxt, "LH : valeur faible dans un contexte de valeurs fortes")
  }else{
    notetxt<-""
  }
  morlm <- lm(wzx ~ zx)
  imoran <- morlm$coefficients[2]
  par(pty="s")

  Graphique <- ggplot(mapping = aes(x=zx, y=wzx, colour=typo)) +
    geom_point() +
    scale_colour_manual(values = couleurs)+
    labs(
      colour = "Typologie",
      title = titre,
      subtitle = paste("I de Moran = ", format(round(imoran,4))),
      caption = notetxt) +
    geom_vline(xintercept = 0, colour="black", linetype="dashed", size=.5) +
    geom_hline(yintercept = 0, colour="black", linetype="dashed", size=.5) +
    stat_smooth(method="lm", se=FALSE, size=1, colour="black") +
    xlab(titreAxeX) +
    ylab(titreAxeY)
  return(Graphique)
}



#' Fonction pour créer le diagramme de Moran dans un contexte bivariée
#' @param x un vecteur avec la variable continue X.
#' @param y un vecteur avec la variable continue X.
#' @param listW un objet listw de spdep (matrice de pondération spatiale).
#' @param titre du graphique
#' @param titreAxeX titre de l'axe des X
#' @param titreAxeY titre de l'axe des X
#' @param AfficheAide indique l'affichage de l'aide de lecture dans le graphique
#' @return un objet ggplot2.
DiagMoranBivarie <- function(x,
                             y,
                             listW,
                             titre="Diagramme de Moran",
                             titreAxeX = "X",
                             titreAxeY = "Variable Y spatialement décalée",
                             AfficheAide=FALSE)
{
  zx <- (x - mean(x))/sd(x)     # variable X centrée réduite (cote z)
  zy <- (y - mean(y))/sd(y)     # variable y centrée réduite (cote z)
  wzy <- lag.listw(listW,zy)    # variable y centrée réduite spatialement décalée

  typo <- ifelse(zx>0 & wzy>0, "HH", "ND")
  typo <- ifelse(zx<0 & wzy<0, "LL", typo)
  typo <- ifelse(zx>0 & wzy<0, "HL", typo)
  typo <- ifelse(zx<0 & wzy>0, "LH", typo)

  couleurs <- c("HH" = "darkred",
                "LL" = "darkblue",
                "HL" = "red",
                "LH" = "blue")

  if (AfficheAide){
    notetxt = "Autocorrélation positive\n"
    notetxt = paste(notetxt, "HH : valeur forte dans un contexte de valeurs fortes\n")
    notetxt = paste(notetxt, "LL : valeur faible dans un contexte de valeurs faibles\n")
    notetxt = paste(notetxt, "Autocorrélation négative\n")
    notetxt = paste(notetxt, "HL : valeur forte dans un contexte de valeurs faibles\n")
    notetxt = paste(notetxt, "LH : valeur faible dans un contexte de valeurs fortes")
  }else{
    notetxt<-""
  }
  morlm <- lm(wzy ~ zx)
  imoran <- morlm$coefficients[2]
  par(pty="s")

  Graphique <- ggplot(mapping = aes(x=zx, y=wzy, colour=typo)) +
    geom_point() +
    scale_colour_manual(values = couleurs)+
    labs(
      colour = "Typologie",
      title = titre,
      subtitle = paste("I de Moran = ", format(round(imoran,4))),
      caption = notetxt) +
    geom_vline(xintercept = 0, colour="black", linetype="dashed", size=.5) +
    geom_hline(yintercept = 0, colour="black", linetype="dashed", size=.5) +
    stat_smooth(method="lm", se=FALSE, size=1, colour="black") +
    xlab(titreAxeX) +
    ylab(titreAxeY)
  return(Graphique)
}
