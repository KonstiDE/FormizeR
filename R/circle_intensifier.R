plot_intensity_bubbles <- function(
  point_layer,
  shape_layer,
  cellsize,
  hex=TRUE,
  hex.border=TRUE,
  hex.border.color="black",
  hex.border.width=0.1,
  plot=TRUE,
  plot.colors=c("white", "blue"),
  plot.scalename="",
  plot.theme=theme_classic(),
  plot.3d=FALSE,
  plot.3d.scale=100,
  plot.3d.sunangle=360,
  plot.3d.shadow_intensity=0.75
){
  point_layer = st_read("data/ger_bakeries.gpkg")
  shape_layer = st_read("data/ger_admin.gpkg")

  #point_layer_buffered = st_buffer(point_layer, 1000, endCapStyle = 'ROUND')
  #counts <- st_contains(point_layer_buffered, point_layer, sparse = F)

  spatialrisk

  ggplot(data = point_layer, aes(fill=counts)) +
    geom_sf() +
    scale_fill_gradientn(colours = c("purple", "orange", "red"))

}