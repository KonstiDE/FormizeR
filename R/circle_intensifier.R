#' @export
#' @title: plot_intensity_bubbles
#' Calculate and plot intensity maps with a circular shape.
#'
#' @param point_layer sf object: An sf object containing points.
#' @param shape_layer sf object: An sf object consisting of a polygon.
#' @param shape logical: Determines if you want to plot the shape behind the bubbles
#' @param shape.fill.color character: Fill color of the shape
#' @param shape.border.color character: Border color of the shape
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
#' plot_intensity_bubbles(ger_points, ger_shape)
#' @import sf
#' @import ggplot2
#' @import rayshader
#' @import rgl
#' @import spatialrisk
plot_intensity_bubbles <- function(
  point_layer,
  shape_layer,
  shape=TRUE,
  shape.fill.color="grey",
  shape.border.color="black",
  plot=TRUE,
  plot.colors=c("white", "blue"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  grid_crs <- crs(point_layer)

  coord_df <- st_coordinates(point_layer$geom)
  coord_df <- coord_df[,1:2]
  coord_df <- as.data.frame(coord_df)
  colnames(coord_df) <- c("lon", "lat")
  coord_df$lon <- as.numeric(coord_df$lon)
  coord_df$lat <- as.numeric(coord_df$lat)

  intensity_list <- numeric()
  for (i in seq_along(coord_df[, 1])){
    intensity_list <- append(
      intensity_list,
      length(spatialrisk::points_in_circle(coord_df, lon_center=coord_df$lon[i], lat_center=coord_df$lat[i], radius = 10000)[,1])
    )
  }
  coord_df$count <- intensity_list
  coord_df <- coord_df[order(coord_df$count),]

  intensity_list <- coord_df$count

  coord_sf <- st_as_sf(coord_df, coords = c("lon", "lat"), crs = grid_crs)

  if(plot){
    p <- ggplot()

    if(shape){
      p <- p + geom_sf(data = shape_layer, color=shape.border.color, aes(fill=shape.fill.color))
    }
    p <- p + geom_sf(data = coord_sf, aes(color=intensity_list, size = intensity_list)) +
      scale_color_gradientn(colors = plot.colors, name=plot.scalename) +
      plot.theme
    if(plot.3d){
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
    }else{
      p
    }
  }else{
    return(cbind(as.data.frame(coord_sf), intensity_list))
  }

}