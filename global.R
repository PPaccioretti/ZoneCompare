library(shiny)
library(mapedit)
library(mapview)
library(sf)
library(leaflet.extras)
library(sp)
library(raster)
library(spdep)
library(leaflet)
library(leafpop)

#############################################



library(tmap)
library(dplyr)
library(ggplot2)
tmap_mode("view")
source("src/comparacionMedias.R")
source("src/depuracion.R")


rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
tran <- function(geo, ang, center, shift = c(0,0)) ((geo + shift) - center) * 1 * rot(ang * pi / 180) + center


targetVariableText <- "Target varbiale"


DatosEjemplo <- st_as_sf(data.frame("lat"=0, "long"=0), coords = c(1:2), crs = 4326)

