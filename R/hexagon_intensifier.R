#' @export
#' @title: plot_intensity_standard
#' Calculate and plot intensity maps with a hexagonial or rectangular shapes.
#'
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param cellsize numeric: Size of the hexagons / rectangles.
#' @param hex logical: If TRUE, will do hexagon, rectangles if FALSE
#' @param hex.border logical: Determines if borders of the forms will be drawn
#' @param hex.border.color character: Sets the color of the outlines (ignored if hex.border=FALSE)
#' @param hex.border.width numeric: Sets the width of the outlines (ignored if hex.border=FALSE)
#' @param plot logical: Whether to plot the map
#' @param plot.color vector of characters: Sets the colorscale for the plot
#' @param plot.scalename character: Displays a name for the scalebar
#' @returns data.frame: With column geometry (sf polygons) and intensity (numerics)
#' @examples
#' plot_intensity_standard(point_layer, shape_layer, cellsize=0.3, hex=TRUE, plot=TRUE)
plot_intensity_standard <- function(point_layer, shape_layer, cellsize, hex=TRUE, hex.border=TRUE, hex.border.color="black", hex.border.width=0.1, plot=TRUE, plot.color=c("white", "blue"), plot.scalename=""){
  grid <- st_make_grid(shape_layer, cellsize = cellsize, square = !hex)

  intersection <- lengths(st_intersects(grid, shape_layer)) > 0
  intensity <- lengths(st_intersects(grid[intersection], point_layer))

  if(plot){
    ggplot(grid[intersection], aes(fill = intensity)) +
      geom_sf(color=if(hex.border) hex.border.color else NA, lwd=hex.border.width) +
      scale_fill_gradientn(colours=plot.color, name=plot.scalename)
  }else{
    return(cbind(as.data.frame(grid[intersection]), intensity))
  }

}