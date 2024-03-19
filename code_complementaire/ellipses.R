#' Fonction pour le calcul de l'ellipse pondérée
#' @param points un objet de type sf avec des points pour coordonnées
#' @param w vecteur pour la pondération (si NULL, les poids sont assumés être 1).
CreateEllipse <- function(points, w = NULL){

  crs <- st_crs(points)
  xy <- st_coordinates(points)
  x <- xy[,1]
  y <- xy[,2]

  if(is.null(w)){
    w <- rep(1,length(x))
  }

  # Coordonnées du centre moyen
  mean.x <- weighted.mean(x,w)
  mean.y <- weighted.mean(y,w)
  # Coordonnées centrées
  zx <- x - mean.x
  zy <- y - mean.y
  sumzx2 <- sum(zx^2 * w)
  sumzy2 <- sum(zy^2 * w)
  sumzxy <- sum(zx * zy * w)
  # Calcul de l'angle de rotation
  numerateur   <- (sumzx2-sumzy2)+ sqrt((sumzx2-sumzy2)^2+ 4*(sumzxy)^2)
  demoninateur <- 2*sumzxy
  tantheta <- numerateur / demoninateur
  # Theta
  theta <- ifelse(tantheta < 0,
                  180 + atan(tantheta)* 180/pi,
                  Theta <- atan(tantheta)* 180/pi)
  sintheta <- sin(theta * pi/180)
  costheta <- cos(theta * pi/180)
  sin2theta <- sintheta^2
  cos2theta <- costheta^2
  sinthetacostheta <- sintheta * costheta
  # Calcul de Sigma.x et Sigma.y
  n <- sum(w)
  sigmax <- sqrt(2) * sqrt(((sumzx2) * (cos2theta) - 2 * (sumzxy) *
                              (sinthetacostheta) + (sumzy2) * (sin2theta))/(n - 2))
  sigmay <- sqrt(2) * sqrt(((sumzx2) * (sin2theta) + 2 * (sumzxy) *
                              (sinthetacostheta) + (sumzy2) * (cos2theta))/(n - 2))
  # Theta corrigé
  if (sigmax > sigmay) {
    Theta.Corr <- theta + 90
    Major <- "SigmaX"
    Minor <- "SigmaY"
  }
  else {
    Theta.Corr <- theta
    Major <- "SigmaY"
    Minor <- "SigmaX"
  }
  # Coordonnées
  B <- min(sigmax, sigmay)
  A <- max(sigmax, sigmay)
  d2 <- (A - B) * (A + B)
  phi <- 2 * pi * seq(0, 1, len = 360)
  sp <- sin(phi)
  cp <- cos(phi)
  r <- sigmax * sigmay/sqrt(B^2 + d2 * sp^2)
  xy <- r * cbind(cp, sp)
  al <- (90 - theta) * pi/180
  ca <- cos(al)
  sa <- sin(al)
  coordsSDE <- xy %*% rbind(c(ca, sa), c(-sa, ca)) +
    cbind(rep(mean.x, 360), rep(mean.y, 360))

  # on s'assure que les dernières coordonnées soient les mêmes que le premières
  coordsSDE[nrow(coordsSDE),] <- coordsSDE[1,]

  # Polygone
  EllPoly <- st_polygon(list(as.matrix(coordsSDE)))
  ellipse <- st_sfc(EllPoly, crs=crs)
  ellipse <- st_sf(data.frame(CMx = mean.x,
                              CMy = mean.y,
                              Sigmax = sigmax,
                              Sigmay = sigmay,
                              Lx = 2*sigmax,
                              Ly = 2*sigmay,
                              Aire = pi*sigmax*sigmay,
                              Theta = theta,
                              ThetaCorr =Theta.Corr,
                              Major = Major,
                              Minor = Minor),
                   geometry = ellipse)
  return(ellipse)
}

#' Fonction pour le calcul de l'ellipse pondérée par groupe
#' @param points un objet de type sf avec des points pour coordonnées
#' @param group une colonne de points indiquant le groupe auquel chaque point appartient
#' @param w vecteur pour la pondération (si NULL, les poids sont assumés être 1).
CreateEllipse_gp <- function(points, group, w = NULL){

  if(is.null(w)){
    w <- rep(1,nrow(points))
  }
  values <- sort(unique(points[[group]]))

  ellipses <- lapply(values, function(x){
    sub <- subset(points, points[[group]] == x)
    w_sub <- w[points[[group]] == x]
    ell <- CreateEllipse(sub, w_sub)
  })
  ellipses <- do.call(rbind,ellipses)
  ellipses[[group]] <- values
  return(ellipses)
}
