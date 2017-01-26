# Fonctions utilises pour recalculer les donnees et les traiter

library(sp)
library(rgdal)
library(tmap)
library(raster)
library(stringr)
library(tidyverse)

modifier_historique_prenoms <- function(data, code_avant, annee_limite, props){
# Rajoute des valeurs pour des départements non présents avant une certaine annee
# Renvoie le jeu de données data modifié
# data doit contenir des colonnes `annee` (numeric), `code_insee` (chr),
# `prenom` (chr), `sexe` (factor M/F)  
# code_avant  est un vecteur de character de taille 1
# props est un tibble `code_insee`, `prop`   

  codes_apres <- props$code_insee
  
  avant <- data %>% 
    filter(code_insee == code_avant,
           annee < annee_limite) %>% 
    complete(nesting(annee, prenom, sexe), code_insee = codes_apres) %>% 
    fill(nombre) %>% 
    inner_join(props, by = "code_insee") %>%
    mutate(nombre = prop * nombre) %>%
    select(-prop)
  
  apres <- data%>% 
    filter(annee >= annee_limite,
           code_insee %in% codes_apres)
  
  data %>% 
    anti_join(props, by = "code_insee") %>% 
    bind_rows(avant, apres) %>% 
    arrange(annee, code_insee, sexe, prenom)
}

modifier_historique_oise <- function(data){
# modifer historique appliqué aux départements 78, 91 et 95
# Les proportions viennent de la population entre 1968 et 1998 (simplifiées)  
  
  prop <-tibble("code_insee" = c("78", "91", "95"),
                     "prop" = c(0.4, 0.3, 0.3))
  
  modifier_historique_prenoms(data, code_avant = "78",
                              annee_limite = 1968,
                              props = prop)
}  

modifier_historique_seine <- function(data){
# modifer historique appliqué aux départements 75, 92, 93 et 94
# Les proportions viennent de la population après 1968 (simplifiées)   
  
  prop <- tibble("code_insee" = c("75", "92", "93", "94"),
                       "prop" = c(0.4, 0.2, 0.2, 0.2))
  
  modifier_historique_prenoms(data, code_avant = "75",
                              annee_limite = 1968,
                              props = prop)
}

modifier_historique_corse <- function(data){
# modifer historique appliqué aux départements 2A et 2B
# Il faut remplacer le département 20 par 2A et 2B (moitié-moitié)
  
  prop <- tibble("code_insee" = c("20", "2A", "2B"),
                       "prop" = c(0, 0.5, 0.5))
  
  modifier_historique_prenoms(data, code_avant = "20",
                              annee_limite = 9999,
                              props = prop) %>% 
    filter(nombre != 0)
}

modifier_historique_tous <- function(data) {
  data %>% 
  modifier_historique_corse() %>% 
  modifier_historique_oise() %>% 
  modifier_historique_seine()
}

calculer_prop <- function(Prenom, debut = 1900, fin = 2015){
# Calule la proportion d'un prénom parmi toutes les naissances par département
# entre les années debut et fin (optionnel)
#  
# Nécessite dans l'environnement global naissances_histo et prenoms_recalcules 
  
  naissances_filtre <- naissances_histo %>% 
    filter(between(annee, debut, fin)) %>% 
    group_by(code_insee) %>% 
    summarise(naissances = sum(naissances))
  
  prenoms_recalcules %>%
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin)) %>%
    group_by(code_insee) %>%
    summarise(total = sum(nombre)) %>%
    inner_join(naissances_filtre, by = "code_insee") %>%
    mutate(prop = total/naissances * 100)
}

creer_carte <- function(Prenom, debut = 1900, fin = 2015){
# Créer une carte de la proportion d'un prénom par département
# 
# Nécessite un jeu de données spatiales france dans l'environnement global
  
  data <- sp::merge(france, calculer_prop(Prenom, debut, fin))
  
  tm_shape(data) +
    tm_borders(alpha = 0.5) +
    tm_fill(col = "prop", 
            id = "nom_dept", 
            textNA = "Aucune",
            title = "En %",
            popup.vars = c("total", "prop"),
            legend.format = list(text.separator = "à")) +
    tm_view(set.zoom.limits = c(5, 9), legend.position = c("left", "bottom")) +
    tm_layout(title = str_c(str_to_title(Prenom), " entre ", debut, " et ", fin))
}