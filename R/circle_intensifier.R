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

  grid_crs <- crs(point_layer)

  #point_layer_buffered = st_buffer(point_layer, 5000, endCapStyle = 'ROUND')
  #counts <- st_contains(point_layer_buffered, point_layer, sparse = F)

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

  coord_sf <- st_as_sf(coord_df, coords = c("lon", "lat"), crs = grid_crs)

  ggplot() +
    geom_sf(data = coord_sf, aes(color=intensity_list, size = intensity_list)) +
    scale_color_gradientn(colors = c("grey", "orange", "red"))

}