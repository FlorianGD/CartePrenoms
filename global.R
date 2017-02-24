library(readr)
library(rgdal)
library(leaflet)
options(OutDec= ",")

naissances <- read_csv("data/recalc/naissances.csv", 
                       col_types = cols(
                         code_insee = col_character(),
                         annee = col_integer(),
                         naissances = col_double()
                       ))
prenoms <- read_csv("data/recalc/prenoms.csv", 
                    col_types = cols(
                      sexe = col_character(),
                      prenom = col_character(),
                      annee = col_integer(),
                      code_insee = col_character(),
                      nombre = col_double()
                    ))
france <- readOGR("data/recalc/departements", "departements-20140306-100m",
                  stringsAsFactors = FALSE)
