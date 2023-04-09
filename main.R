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

load_all("R/")


p <- plot_intensity_rectengular_fishernet(
  ger_points,
  ger_admin,
  cellsize = 0.7,
  net.border = FALSE,
  plot.colors = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"),
  plot.theme = theme_dark()
)

p

snapshot3d("readme/hex_3d.png")


#df <- plot_intensity_finshernet(
#  point_layer,
#  shape_layer,
#  cellsize = 0.4,
#  net.border = FALSE,
#  plot = FALSE
#)




