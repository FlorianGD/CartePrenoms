library(sp)
library(rgdal)
library(tmap)
library(raster)
library(dplyr)
library(readr)

naissances <- readRDS("data/recalc/naissances.rds")
prenoms <- readRDS("data/recalc/prenoms.rds")
france <- readOGR("data/recalc/departements", "departements-20140306-100m",
                 stringsAsFactors = FALSE)