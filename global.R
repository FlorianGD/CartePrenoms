library(sp)
library(rgdal)
library(tmap)
library(raster)
library(tidyverse)

naissances <- read_csv("data/recalc/naissances.csv")
prenoms <- read_csv("data/recalc/prenoms_recalcules.csv")
france <- readOGR("data/recalc/departements", "departements-20140306-100m",
                 stringsAsFactors = FALSE)