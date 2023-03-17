# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

install.packages("devtools")
install.packages("sf")
install.packages("ggplot2")

source("R/hexagon_intensifier.R")
source("R/triangle_intensifier.R")

shape_layer <- st_read("data/ger_admin.gpkg")
point_layer <- st_read("data/ger_bakeries.gpkg")

plot_intensity_standard(
  point_layer,
  shape_layer,
  cellsize = 0.2,
  hex = TRUE,
  hex.border = FALSE,
  plot=TRUE
)

df <- plot_intensity_finshernet(
  point_layer,
  shape_layer,
  cellsize = 0.4,
  net.border = FALSE,
  plot = FALSE
)

ggplot(df$geometry, aes(fill=df$intensity)) +
  geom_sf() +
  scale_fill_gradientn(colours=c("black", "orange"))




