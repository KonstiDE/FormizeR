#' @export
#' @title: plot_intensity_standard
#' Calculate and plot intensity maps with a hexagonial or rectangular shape.
#'
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the hexagons / rectangles.
#' @param hex logical: If TRUE, will do hexagon, rectangles if FALSE
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
#' plot_intensity_standard(ger_points, ger_shape, cellsize=0.3, hex=TRUE, plot=TRUE)
#' @import sf
#' @import ggplot2
#' @import rayshader
#' @import rgl
plot_intensity_standard <- function(
  point_layer,
  shape_layer,
  cellsize,
  hex=TRUE,
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
  grid <- st_make_grid(shape_layer, cellsize = cellsize, square = !hex)

  intersection <- lengths(st_intersects(grid, shape_layer)) > 0
  intensity <- lengths(st_intersects(grid[intersection], point_layer))

  if(plot){
    p <- ggplot(grid[intersection], aes(fill = intensity)) +
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
    return(cbind(as.data.frame(grid[intersection]), intensity))
  }

}