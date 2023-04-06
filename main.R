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


plot_intensity_rhombus(
  ger_points,
  ger_admin,
  cellsize = 1,
  plot.color = c("grey", "orange", "red"),
  plot.theme = theme_classic()
)

snapshot3d("readme/hex_3d.png")


#df <- plot_intensity_finshernet(
#  point_layer,
#  shape_layer,
#  cellsize = 0.4,
#  net.border = FALSE,
#  plot = FALSE
#)




