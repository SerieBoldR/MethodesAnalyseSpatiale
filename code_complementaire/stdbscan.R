#' Fonction pour l'algorithme st-dbscan
#' Source : https://github.com/CKerouanton/ST-DBSCAN_sensitivity
#' @param x vecteur pour les coordonnées X des points.
#' @param y vecteur pour les coordonnées Y des points.
#' @param eps1 valeur de epsilon pour la distance.
#' @param eps2 valeur de epsilon pour le temps.
#' @param minpts nombre de points minimum pour les points centraux.

stdbscan = function (x, y, time, eps1, eps2, minpts) {
  countmode = 1:length(x)
  seeds = TRUE
  data_spatial  <- as.matrix(dist(cbind(y, x)))
  data_temporal <- as.matrix(dist(time))
  n <- nrow(data_spatial)
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)

  for (i in 1:n) {
    if (i %in% countmode)
      #cat("Processing point ", i, " of ", n, ".\n")
      unclass <- (1:n)[cv < 1]

    if (cv[i] == 0) {
      reachables <- intersect(unclass[data_spatial[i, unclass] <= eps1],
                              unclass[data_temporal[i, unclass] <= eps2])
      if (length(reachables) + classn[i] < minpts)
        cv[i] <- (-1)
      else {
        cn <- cn + 1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()

          for (i2 in seq(along = ap)) {
            j <- ap[i2]

            jreachables <- intersect(unclass[data_spatial[j, unclass] <= eps1],
                                     unclass[data_temporal[j, unclass] <= eps2])

            if (length(jreachables) + classn[j] >= minpts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 0])
            }
            classn[jreachables] <- classn[jreachables] + 1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break

  }


  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  out <- list(cluster = cv, eps1 = eps1, eps2 = eps2, minpts = minpts, density = classn)
  rm(classn)
  if (seeds && cn > 0) {
    out$isseed <- isseed
  }
  class(out) <- "stdbscan"
  return(out)
}
