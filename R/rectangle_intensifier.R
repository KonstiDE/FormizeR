position_diamond <- function(m, crs, offset_x, offset_y){
  if(offset_x != 0){
    m[1,][1] <- m[1,][1] + offset_x
    m[2,][1] <- m[2,][1] + offset_x
    m[3,][1] <- m[3,][1] + offset_x
    m[4,][1] <- m[4,][1] + offset_x
    m[5,][1] <- m[5,][1] + offset_x
  }
  if(offset_y != 0){
    m[1,][2] <- m[1,][2] + offset_y
    m[2,][2] <- m[2,][2] + offset_y
    m[3,][2] <- m[3,][2] + offset_y
    m[4,][2] <- m[4,][2] + offset_y
    m[5,][2] <- m[5,][2] + offset_y
  }
  return(st_sfc(st_polygon(list(m)), crs=crs))
}

position_rectengular_fisher <- function(m, crs, offset_x, offset_y){
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
#' @title: plot_intensity_diamond
#' Calculate and plot intensity maps with a diamond shape.
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the diamonds of the net.
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
#' plot_intensity_diamond(point_layer, shape_layer, cellsize=0.3, hex=TRUE, plot=TRUE)
#' @import raster
#' @import sf
#' @import ggplot2
#' @import rayshader
#' @import rgl
plot_intensity_diamond <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.border=TRUE,
  net.border.color="black",
  net.border.width=1,
  plot=TRUE,
  plot.colors=c("grey", "orange", "red"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  lonlat1 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin)
  lonlat2 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + cellsize / 2)
  lonlat3 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize)
  lonlat4 <- cbind(grid_extent@xmin, grid_extent@ymin + cellsize / 2)

  matrix <- rbind(lonlat1, lonlat2, lonlat3, lonlat4, lonlat1)

  diamond_list <- list()
  starting_x <- 0
  runner_x <- 0
  runner_y <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / cellsize * 2))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize))){
      diamond_list[[length(diamond_list) + 1]] <- position_diamond(matrix, grid_crs, runner_x + starting_x, runner_y)
      runner_x <- runner_x + cellsize
    }
    runner_x <- 0
    runner_y <- runner_y + cellsize / 2
    if(starting_x == 0){
      starting_x <- cellsize / 2
    }else{
      starting_x <- 0
    }
  }
  diamond_sf <- sf::st_sf(as.data.frame(do.call(rbind, diamond_list)), crs = grid_crs)
  diamond_sf <- sf::st_as_sfc(diamond_sf)

  intersection <- lengths(st_intersects(diamond_sf, shape_layer)) > 0
  intensity <- lengths(st_intersects(diamond_sf[intersection], point_layer))

  if(plot){
    p <- ggplot(diamond_sf[intersection], aes(fill = intensity)) +
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
#' @title: plot_intensity_rectengular_fishernet
#' Calculate and plot intensity maps with a finshernet-triangular shape.
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the diamonds of the net.
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
#' plot_intensity_rectengular_fishernet(point_layer, shape_layer, cellsize=0.3, hex=TRUE, plot=TRUE)
#' @import raster
#' @import sf
#' @import ggplot2
#' @import rayshader
#' @import rgl
plot_intensity_rectengular_fishernet <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.border=TRUE,
  net.border.color="black",
  net.border.width=1,
  plot=TRUE,
  plot.colors=c("grey", "orange", "red"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  lonlat1 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat2 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat3 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)

  lonlat4 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat5 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + cellsize)
  lonlat6 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)

  lonlat7 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + cellsize)
  lonlat8 <- cbind(grid_extent@xmin, grid_extent@ymin + cellsize)
  lonlat9 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)

  lonlat10 <- cbind(grid_extent@xmin, grid_extent@ymin + cellsize)
  lonlat11 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat12 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)

  matrix1 <- rbind(lonlat1, lonlat2, lonlat3, lonlat1)
  matrix2 <- rbind(lonlat4, lonlat5, lonlat6, lonlat4)
  matrix3 <- rbind(lonlat7, lonlat8, lonlat9, lonlat7)
  matrix4 <- rbind(lonlat10, lonlat11, lonlat12, lonlat10)

  rectfisher_list <- list()
  runner_x <- 0
  runner_y <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / cellsize * 2))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize))){
      lenght_list <- length(rectfisher_list)
      rectfisher_list[[lenght_list + 1]] <- position_rectengular_fisher(matrix1, grid_crs, runner_x, runner_y)
      rectfisher_list[[lenght_list + 2]] <- position_rectengular_fisher(matrix2, grid_crs, runner_x, runner_y)
      rectfisher_list[[lenght_list + 3]] <- position_rectengular_fisher(matrix3, grid_crs, runner_x, runner_y)
      rectfisher_list[[lenght_list + 4]] <- position_rectengular_fisher(matrix4, grid_crs, runner_x, runner_y)
      runner_x <- runner_x + cellsize
    }
    runner_x <- 0
    runner_y <- runner_y + cellsize
  }
  rectfisher_sf <- sf::st_sf(as.data.frame(do.call(rbind, rectfisher_list)), crs = grid_crs)
  rectfisher_sf <- sf::st_as_sfc(rectfisher_sf)

  intersection <- lengths(st_intersects(rectfisher_sf, shape_layer)) > 0
  intensity <- lengths(st_intersects(rectfisher_sf[intersection], point_layer))

  if(plot){
    p <- ggplot(rectfisher_list[intersection], aes(fill = intensity)) +
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

