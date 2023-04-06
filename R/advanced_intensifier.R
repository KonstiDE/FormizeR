position_rhombus_tile <- function(m, crs, offset_x, offset_y){
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



#' @export
#' @title: plot_intensity_rhombus
#' Calculate and plot intensity maps with a rhombus tiled shape.
#'
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the hexagons / rectangles.
#' @param net.alpha numeric: Between 0 and 1 for opacity controlling
#' @param net.border logical: Determines if borders of the forms will be drawn
#' @param net.border.color character: Sets the color of the outlines (ignored if hex.border=FALSE)
#' @param net.border.width numeric: Sets the width of the outlines (ignored if hex.border=FALSE)
#' @param plot logical: Whether to plot the map
#' @param plot.colors vector of characters: Sets the colorscale for the plot
#' @param plot.scalename character: Displays a name for the scalebar
#' @param plot.theme ggplot2 theme: Select a theme
#' @param plot.3d logical: Plots the image in a 3d environment (ignored if plot=FALSE)
#' @param plot.3d.scale logical: Plots the image in a 3d environment (ignored if plot.3d=FALSE)
#' @param plot.3d.sunangle numeric: Angle of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @param plot.3d.shadow_intensity numeric: Intensity of the shadow in the 3d plot (ignored if plot.3d=FALSE)
#' @returns data.frame: With column geometry (sf polygons) and intensity (numerics)
#' @examples
#' plot_intensity_rhombus(ger_points, ger_shape, cellsize=0.3, plot=TRUE)
#' @import sf
#' @import ggplot2
#' @import rayshader
#' @import rgl
plot_intensity_rhombus <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.alpha=1.0,
  net.border=TRUE,
  net.border.color="black",
  net.border.width=0.1,
  plot=TRUE,
  plot.colors=c("white", "blue"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  lonlat1 <- cbind(grid_extent@xmin, grid_extent@ymin + cellsize / 4)
  lonlat2 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin)
  lonlat3 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)
  lonlat4 <- cbind(grid_extent@xmin, grid_extent@ymin + 3 * cellsize / 4)
  pol_matrix <- rbind(lonlat1, lonlat2, lonlat3, lonlat4, lonlat1)

  lonlat5 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin)
  lonlat6 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + cellsize / 4)
  lonlat7 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + 3 * cellsize / 4)
  lonlat8 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)
  pol2_matrix <- rbind(lonlat5, lonlat6, lonlat7, lonlat8, lonlat5)

  lonlat9 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize / 2)
  lonlat10 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin + 3 * cellsize / 4)
  lonlat11 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + cellsize)
  lonlat12 <- cbind(grid_extent@xmin, grid_extent@ymin + 3 * cellsize / 4)
  pol3_matrix <- rbind(lonlat9, lonlat10, lonlat11, lonlat12, lonlat9)

  rhombus_list <- list()
  runner_x <- 0
  runner_y <- 0
  starting_x <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / cellsize))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize))){
      rhombus_list[[length(rhombus_list) + 1]] <- position_rhombus_tile(pol_matrix, grid_crs, runner_x + starting_x, runner_y)
      rhombus_list[[length(rhombus_list) + 1]] <- position_rhombus_tile(pol2_matrix, grid_crs, runner_x + starting_x, runner_y)
      rhombus_list[[length(rhombus_list) + 1]] <- position_rhombus_tile(pol3_matrix, grid_crs, runner_x + starting_x, runner_y)

      runner_x <- runner_x + cellsize
    }
    runner_x <- 0
    runner_y <- runner_y + 3 * cellsize / 4
    if(starting_x == 0){
      starting_x <- cellsize / 2
    }else{
      starting_x <- 0
    }
  }
  rhombus_sf <- sf::st_sf(as.data.frame(do.call(rbind, rhombus_list)), crs = grid_crs)
  rhombus_sf <- sf::st_as_sfc(rhombus_sf)

  intersection <- lengths(st_intersects(rhombus_sf, shape_layer)) > 0
  intensity <- lengths(st_intersects(rhombus_sf[intersection], point_layer))

  if(plot){
    p <- ggplot(rhombus_sf[intersection], aes(fill = intensity)) +
      geom_sf(color=if(net.border) net.border.color else NA, lwd=net.border.width, alpha = net.alpha) +
      scale_fill_gradientn(colours=plot.colors, name=plot.scalename) +
      plot.theme

    if(!plot.3d){
      p
    }else{
      open3d()
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
    return(cbind(as.data.frame(rhombus_sf[intersection]), intensity))
  }

}


#' @export
#' @title: plot_intensity_trakistile
#' Calculate and plot intensity maps with a trakis-triangular shape.
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the triangles of the net.
#' @param net.alpha numeric: Between 0 and 1 for opacity controlling
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
#' plot_intensity_trakistile(ger_points, ger_shape, cellsize=0.3, plot=TRUE)
#' @import raster
#' @import sf
#' @import ggplot2
#' @import rayshader
#' @import rgl
plot_intensity_trakistile <- function(
  point_layer,
  shape_layer,
  cellsize,
  net.alpha=1.0,
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

  cellsize <- 1

  height <- sqrt(cellsize * cellsize - (cellsize / 2) * (cellsize / 2))
  height

  grid_extent <- extent(point_layer)
  grid_crs <- crs(point_layer)

  lonlat1 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat2 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat3 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + height / 2.666)
  matrix <- rbind(lonlat1, lonlat2, lonlat3, lonlat1)

  lonlat4 <- cbind(grid_extent@xmin + cellsize, grid_extent@ymin)
  lonlat5 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + height)
  lonlat6 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + height / 2.666)
  matrix2 <- rbind(lonlat4, lonlat5, lonlat6, lonlat4)

  lonlat7 <- cbind(grid_extent@xmin, grid_extent@ymin)
  lonlat8 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + height / 2.666)
  lonlat9 <- cbind(grid_extent@xmin + cellsize / 2, grid_extent@ymin + height)
  matrix3 <- rbind(lonlat7, lonlat8, lonlat9, lonlat7)


  ggplot() +
    geom_sf(data = st_sfc(st_polygon(list(matrix)))) +
    geom_sf(data = st_sfc(st_polygon(list(matrix2)))) +
    geom_sf(data = st_sfc(st_polygon(list(matrix3))))


  fishernet_list <- list()
  runner_x <- 0
  runner_y <- 0
  for (t in seq(0:ceiling((grid_extent@ymax - grid_extent@ymin) / height))){
    for (i in seq(0:ceiling((grid_extent@xmax - grid_extent@xmin) / cellsize * 2))){
      fishernet_list[[length(fishernet_list) + 1]] <- position_triangle_midi(matrix, grid_crs, height, runner_x, runner_y, if(t %% 2) i %% 2 == 0 else i %% 2 == 1)
      fishernet_list[[length(fishernet_list) + 1]] <- position_triangle_midi(matrix, grid_crs, height, runner_x, runner_y, if(t %% 2) i %% 2 == 0 else i %% 2 == 1)
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
      geom_sf(color=if(net.border) net.border.color else NA, lwd=net.border.width, alpha=net.alpha) +
      scale_fill_gradientn(colours=plot.colors, name=plot.scalename) +
      plot.theme

    if(!plot.3d){
      p
    }else{
      open3d()
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

