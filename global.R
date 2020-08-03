library(shiny)
library(mapedit)
library(mapview)
library(sf)



library(mapview)
library(leaflet)
library(shiny)
library(ggplot2)
library(mapedit)
library(leaflet.extras)
library(sf)



#############################################


library(sf)
library(tmap)
library(dplyr)
library(ggplot2)
tmap_mode("view")
source("src/comparacionMedias.R")
source("src/depuracion.R")


rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
tran <- function(geo, ang, center, shift = c(0,0)) ((geo + shift) - center) * 1 * rot(ang * pi / 180) + center


# DatosRto <- st_read("src/datos", crs = 4326, quiet = TRUE)
# DatosRto <- st_transform(DatosRto, 32721)
# DatosRto <- DatosRto[sample(nrow(DatosRto),1000),]



targetVariableText <- "Target varbiale"