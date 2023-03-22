position_triangle_midi <- function(m, crs, height, offset_x, offset_y, flip){
  if(flip){
    m[1,][2] <- m[1,][2] + height
    m[4,][2] <- m[4,][2] + height
    m[3,][2] <- m[3,][2] - height
    m[2,][2] <- m[2,][2] + height
  }
  if(offset_x != 0){
    m[1,][1] <- m[1,][1] + offset_x
    m[2,][1] <- m[2,][1] + offset_x
    m[3,][1] <- m[3,][1] + offset_x
    m[4,][1] <- m[4,][1] + offset_x
  }
  if(offset_y != 0){
    m[1,][2] <- m[1,][2] + offset_y
    m[2,][2] <- m[2,][2] + offset_y
    m[3,][2] <- m[3,][2] + offset_y
    m[4,][2] <- m[4,][2] + offset_y
  }
  return(st_sfc(st_polygon(list(m)), crs=crs))
}

position_triangle_left <- function(m, crs, cellsize, height, offset_x, offset_y, flip){
  if(flip){
    m[1,][1] <- m[1,][1] + cellsize
    m[1,][2] <- m[1,][2] + height
    m[4,][1] <- m[4,][1] + cellsize
    m[4,][2] <- m[4,][2] + height
  }
  if(offset_x != 0){
    m[1,][1] <- m[1,][1] + offset_x
    m[2,][1] <- m[2,][1] + offset_x
    m[3,][1] <- m[3,][1] + offset_x
    m[4,][1] <- m[4,][1] + offset_x
  }
  if(offset_y != 0){
    m[1,][2] <- m[1,][2] + offset_y
    m[2,][2] <- m[2,][2] + offset_y
    m[3,][2] <- m[3,][2] + offset_y
    m[4,][2] <- m[4,][2] + offset_y
  }
  return(st_sfc(st_polygon(list(m)), crs=crs))
}

position_triangle_right <- function(m, crs, cellsize, height, offset_x, offset_y, flip){
  if(flip){
    m[2,][1] <- m[2,][1] - cellsize
    m[2,][2] <- m[2,][2] + height
  }
  if(offset_x != 0){
    m[1,][1] <- m[1,][1] + offset_x
    m[2,][1] <- m[2,][1] + offset_x
    m[3,][1] <- m[3,][1] + offset_x
    m[4,][1] <- m[4,][1] + offset_x
  }
  if(offset_y != 0){
    m[1,][2] <- m[1,][2] + offset_y
    m[2,][2] <- m[2,][2] + offset_y
    m[3,][2] <- m[3,][2] + offset_y
    m[4,][2] <- m[4,][2] + offset_y
  }
  return(st_sfc(st_polygon(list(m)), crs=crs))
}


#' @export
#' @title: plot_intensity_fishernet
#' Calculate and plot intensity maps with a finshernet-triangular shape.
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the triangles of the net.
#' @param net.border logical: Determines if borders of the forms will be drawn
#' @param net.border.color character: Sets the color of the outlines (ignored if hex.border=FALSE)
#' @param net.border.width numeric: Sets the width of the outlines (ignored if hex.border=FALSE)
#' @param plot logical: Whether to plot the map
#' @param plot.colors vector of characters: Sets the colorscale for the plot
#' @param plot.scalename character: Displays a name for the scalebar
#' @param plot.3d logical: Plots the image in a 3d environment (ignored if plot=FALSE)
#' @param plot.3d.scale logical: Plots the image in a 3d environment (ignored if plot.3d=FALSE)
#' @param plot.3d.sunangle numeric: Angle of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @param plot.3d.shadow_intensity numeric: Intensity of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @returns data.frame: With column geometry (sf polygons) and intensity (numerics)
#' @examples
#' plot_intensity_finshernet(point_layer, shape_layer, cellsize=0.3, hex=TRUE, plot=TRUE)
plot_intensity_finshernet <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.border=TRUE,
  net.border.color="black",
  net.border.width=1,
  plot=TRUE,
  plot.colors=c("purple", "orange", "red"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  height <- sqrt(cellsize * cellsize - (cellsize / 2) * (cellsize / 2))
  height

  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  lonlat1 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat2 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat3 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + height)

  matrix <- rbind(lonlat1, lonlat2, lonlat3, lonlat1)

  fishernet_list <- list()
  runner_x <- 0
  runner_y <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / height))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize * 2))){
      fishernet_list[[length(fishernet_list) + 1]] <- position_triangle_midi(matrix, grid_crs, height, runner_x, runner_y, if(t %% 2) i %% 2 == 0 else i %% 2 == 1)
      runner_x <- runner_x + cellsize / 2
    }
    runner_x <- 0
    runner_y <- runner_y + height
  }
  fishernet_sf <- sf::st_sf(as.data.frame(do.call(rbind, fishernet_list)), crs = grid_crs)
  fishernet_sf <- sf::st_as_sfc(fishernet_sf)

  intersection <- lengths(st_intersects(fishernet_sf, shape_layer)) > 0
  intensity <- lengths(st_intersects(fishernet_sf[intersection], point_layer))

  if(plot){
    p <- ggplot(fishernet_sf[intersection], aes(fill = intensity)) +
      geom_sf(color=if(net.border) net.border.color else NA, lwd=net.border.width) +
      scale_fill_gradientn(colours=plot.colors, name=plot.scalename) +
      plot.theme

    if(!plot.3d){
      p
    }else{
      rgl.open()
      plot_gg(
        p,
        multicore = T,
        width=5,
        height=5,
        scale=plot.3d.scale,
        shadow_intensity = plot.3d.shadow_intensity,
        offset_edges=T,
        sunangle = plot.3d.sunangle,
        zoom = 0.5,
        phi = 30,
        theta = -30,
      )
    }

  }else{
    return(cbind(as.data.frame(grid[intersection]), intensity))
  }
}


#' @export
#' @title: plot_intensity_triangular_left
#' Calculate and plot intensity maps with a finshernet-triangular shape.
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the triangles of the net.
#' @param net.border logical: Determines if borders of the forms will be drawn
#' @param net.border.color character: Sets the color of the outlines (ignored if hex.border=FALSE)
#' @param net.border.width numeric: Sets the width of the outlines (ignored if hex.border=FALSE)
#' @param plot logical: Whether to plot the map
#' @param plot.colors vector of characters: Sets the colorscale for the plot
#' @param plot.scalename character: Displays a name for the scalebar
#' @param plot.3d logical: Plots the image in a 3d environment (ignored if plot=FALSE)
#' @param plot.3d.scale logical: Plots the image in a 3d environment (ignored if plot.3d=FALSE)
#' @param plot.3d.sunangle numeric: Angle of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @param plot.3d.shadow_intensity numeric: Intensity of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @returns data.frame: With column geometry (sf polygons) and intensity (numerics)
#' @examples
#' plot_intensity_triangular_left(point_layer, shape_layer, cellsize=0.3, hex=TRUE, plot=TRUE)
plot_intensity_triangular_left <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.border=TRUE,
  net.border.color="black",
  net.border.width=1,
  plot=TRUE,
  plot.colors=c("purple", "orange", "red"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  height <- sqrt(cellsize * cellsize - (cellsize / 2) * (cellsize / 2))

  lonlat1 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat2 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat3 <- cbind(grid_extent@xmin, grid_extent@ymin + height)

  matrix <- rbind(lonlat1, lonlat2, lonlat3, lonlat1)

  triangle_list <- list()
  runner_x <- 0
  runner_y <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / cellsize))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize * 2))){
      triangle_list[[length(triangle_list) + 1]] <- position_triangle_left(matrix, grid_crs, cellsize, height, runner_x, runner_y, if(t %% 2) i %% 2 == 0 else i %% 2 == 1)
      if(i %% 2 != 0){
        runner_x <- runner_x + cellsize
      }
    }
    runner_x <- 0
    runner_y <- runner_y + height
  }
  triangle_sf <- sf::st_sf(as.data.frame(do.call(rbind, triangle_list)), crs = grid_crs)
  triangle_sf <- sf::st_as_sfc(triangle_sf)

  intersection <- lengths(st_intersects(triangle_sf, shape_layer)) > 0
  intensity <- lengths(st_intersects(triangle_sf[intersection], point_layer))

  if(plot){
    p <- ggplot(triangle_sf[intersection], aes(fill = intensity)) +
      geom_sf(color=if(net.border) net.border.color else NA, lwd=net.border.width) +
      scale_fill_gradientn(colours=plot.colors, name=plot.scalename) +
      plot.theme

    if(!plot.3d){
      p
    }else{
      rgl.open()
      plot_gg(
        p,
        multicore = T,
        width=5,
        height=5,
        scale=plot.3d.scale,
        shadow_intensity = plot.3d.shadow_intensity,
        offset_edges=T,
        sunangle = plot.3d.sunangle,
        zoom = 0.5,
        phi = 30,
        theta = -30,
      )
    }

  }else{
    return(cbind(as.data.frame(grid[intersection]), intensity))
  }
}


#' @export
#' @title: plot_intensity_triangular_right
#' Calculate and plot intensity maps with a finshernet-triangular shape.
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the triangles of the net.
#' @param net.border logical: Determines if borders of the forms will be drawn
#' @param net.border.color character: Sets the color of the outlines (ignored if hex.border=FALSE)
#' @param net.border.width numeric: Sets the width of the outlines (ignored if hex.border=FALSE)
#' @param plot logical: Whether to plot the map
#' @param plot.colors vector of characters: Sets the colorscale for the plot
#' @param plot.scalename character: Displays a name for the scalebar
#' @param plot.3d logical: Plots the image in a 3d environment (ignored if plot=FALSE)
#' @param plot.3d.scale logical: Plots the image in a 3d environment (ignored if plot.3d=FALSE)
#' @param plot.3d.sunangle numeric: Angle of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @param plot.3d.shadow_intensity numeric: Intensity of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @returns data.frame: With column geometry (sf polygons) and intensity (numerics)
#' @examples
#' plot_intensity_triangular_right(point_layer, shape_layer, cellsize=0.3, hex=TRUE, plot=TRUE)
plot_intensity_triangular_right <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.border=TRUE,
  net.border.color="black",
  net.border.width=1,
  plot=TRUE,
  plot.colors=c("purple", "orange", "red"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  point_layer <- st_read("data/ger_bakeries.gpkg")
  shape_layer <- st_read("data/ger_admin.gpkg")

  height <- sqrt(cellsize * cellsize - (cellsize / 2) * (cellsize / 2))

  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  lonlat1 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat2 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat3 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + height)

  matrix <- rbind(lonlat1, lonlat2, lonlat3, lonlat1)

  triangle_list <- list()
  runner_x <- 0
  runner_y <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / cellsize))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize * 2))){
      triangle_list[[length(triangle_list) + 1]] <- position_triangle_right(matrix, grid_crs, cellsize, height, runner_x, runner_y, if(t %% 2) i %% 2 == 0 else i %% 2 == 1)
      if(i %% 2 == 0){
        runner_x <- runner_x + cellsize
      }
    }
    runner_x <- 0
    runner_y <- runner_y + height
  }
  triangle_sf <- sf::st_sf(as.data.frame(do.call(rbind, triangle_list)), crs = grid_crs)
  triangle_sf <- sf::st_as_sfc(triangle_sf)

  intersection <- lengths(st_intersects(triangle_sf, shape_layer)) > 0
  intensity <- lengths(st_intersects(triangle_sf[intersection], point_layer))

  if(plot){
    p <- ggplot(triangle_sf[intersection], aes(fill = intensity)) +
      geom_sf(color=if(net.border) net.border.color else NA, lwd=net.border.width) +
      scale_fill_gradientn(colours=plot.colors, name=plot.scalename) +
      plot.theme

    if(!plot.3d){
      p
    }else{
      rgl.open()
      plot_gg(
        p,
        multicore = T,
        width=5,
        height=5,
        scale=plot.3d.scale,
        shadow_intensity = plot.3d.shadow_intensity,
        offset_edges=T,
        sunangle = plot.3d.sunangle,
        zoom = 0.5,
        phi = 30,
        theta = -30,
      )
    }

  }else{
    return(cbind(as.data.frame(grid[intersection]), intensity))
  }
}