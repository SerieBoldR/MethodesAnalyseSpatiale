d3_plot_situation <- function(lines, events, pt_samples, densities, scales){

  open3d(scale=scales)

  densities <- round(densities,7)

  ## finding for each event its closest samples
  XY1 <- st_coordinates(pt_samples)
  XY2 <- st_coordinates(events)
  idx <- dbscan::kNN(x = XY1, query = XY2, k = 1)$id
  #idx <- knnx.index(XY1, query = XY2, k = 1)

  events$dens <- densities[idx]
  events$x <- XY2[,1]
  events$y <- XY2[,2]
  eidx <- do.call(c,lapply(1:nrow(events), function(i){c(i,i)}))
  vert_lines <- st_drop_geometry(events[eidx,])
  vert_lines$dens <- ifelse(1:nrow(vert_lines)%%2 == 0, vert_lines$dens,0)


  ## plotting the situation
  #line_coords <- do.call(rbind,unlist(coordinates(lines), recursive = FALSE))
  line_coords <- st_coordinates(lines)
  sample_coords <- st_coordinates(pt_samples)

  segments3d(x=line_coords[,1],
             y=line_coords[,2],
             z=rep(0, nrow(line_coords)))

  segments3d(x=vert_lines$x,
             y=vert_lines$y,
             z=vert_lines$dens,
             col = "red")

  coords_events <- st_coordinates(events)

  points3d(
    x = coords_events[,1],
    y = coords_events[,2],
    z = rep(0,nrow(event)),
    col = "red",
    size = 5
  )

  points3d(
    x = sample_coords[,1],
    y = sample_coords[,2],
    z = densities,
    col = "blue"
  )

  axes3d()
  title3d(xlab="X",ylab="Y",zlab="Z")
}
