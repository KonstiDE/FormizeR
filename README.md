# FormizeR
R package for "Introduction to Programming with R". It easily lets you create 
visual overviews for point intensities with different shapes.

## Installation
For installing the FormizeR package, install the newest of the ``devtools`` or
``remotes`` package. Then go ahead with ``devtools/remotes::install_github("KonstiDE/FormizeR")`` or with and load it via 
``library(FormizeR)``

## Exemplary Usage
Using the FormizeR plugin is easy, as FormizeR directly provides you 
with exemplary data to test our its potential and get familiar with
the styling options. Mandatory are a point layer, a shape layer (both provided inside the package) and a cellsize, 
determining the size of the form. The standard method with minimal parameters goes as follows:

```R
plot_intensity_standard(
    ger_points,                 #provided by FormizeR
    ger_admin,                  #provided by FormizeR
    cellsize = 0.3              #size of the form
)
```

This will plot the bakery-intensity of germany in hexagons, with a default
color scale of white to blue. However, to style your plot, FormizeR provides
you with a lot of parameters. Mandatory parameters are bold and cursive, not mandatory ones with their default
values only bold:

> ***point_layer*** sf object: An sf object containing points.\
> ***shape_layer*** sf object: An sf object consisting of a polygon.\
> ***cellsize*** numeric: Size of the hexagons / rectangles.\
> **hex** logical=TRUE\
> **hex.border** logical=TRUE\
> **hex.border.color** character='black'\
> **hex.border.width** numeric=1\
> **plot** logical=TRUE\
> **plot.color** vector of characters=c("white", "blue")\
> **plot.scalename** character='':

Anyhow, one thinks that the parameters are not sufficient to create the plot
you are aiming for, you simply set ``plot=FALSE`` and collect the dataframe
from ``plot_intensity_[...]`` method and plot in yourself via the famous
``ggplot`` package. The resulting dataframe consists of a geometry and intensity
column containing the polygons and sums of the points for each form respectively:

```R
df <- plot_intensity_standard(
    ger_points,
    ger_shape,
    cellsize = 0.4,
    plot=FALSE
)

ggplot(data = df$geometry, mapping = aes(df$intensity)) + 
    geom_sf(color=NA) + 
    scale_fill_gradient(colours = c("white", "purple", "blue", "black"))
```

The following table depicts examples what can be done with the library with different methods
it provides, such as:

> plot_intensity_standard(...)\
> plot_intensity_fishernet(...)\
> plot_intensity_trinagles_left(...)\
> plot_intensity_triangles_right(...)\


