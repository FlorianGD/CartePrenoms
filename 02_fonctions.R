# Fonctions utilises pour recalculer les donnees et les traiter

library(sp)
library(rgdal)
library(tmap)
library(raster)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

### Fonctions pour créer le jeu de données modifié
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

### Fonctions pour afficher la carte des prénoms
calculer_prop <- function(Prenom, debut = 1900, fin = 2015){
# Calule la proportion d'un prénom parmi toutes les naissances par département
# entre les années debut et fin (optionnel)
#  
# Nécessite dans l'environnement global naissances et prenoms 
  
  naissances_filtre <- naissances %>% 
    filter(between(annee, debut, fin)) %>% 
    group_by(code_insee) %>% 
    summarise(naissances = sum(naissances))
  
  prenoms %>%
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin)) %>%
    group_by(code_insee) %>%
    summarise(total = sum(nombre)) %>%
    inner_join(naissances_filtre, by = "code_insee") %>%
    mutate(prop = total/naissances * 100)
}

creer_carte <- function(Prenom, debut = 1900, fin = 2015, remplissage = "prop"){
# Créer une carte d'un prénom par département, le remplissage par défaut
# est la proportion des naissances.
# Nécessite un jeu de données spatiales france dans l'environnement global

  tm_shape(sp::merge(france, calculer_prop(Prenom, debut, fin))) +
    tm_borders(alpha = 0.5) +
    tm_fill(col = remplissage, 
            id = "nom_dept", 
            title = switch(remplissage,
                           "prop" = "En %",
                           "total" = "Nombre"),
            textNA = "Aucune",
            popup.vars = c("total", "prop"),
            legend.format = list(text.separator = "à",
                                 big.mark = "")) +
    tm_view(set.zoom.limits = c(5, 9), 
            legend.position = c("left", "bottom"))
}

creer_histogramme <- function(Prenom, debut = 1900, fin = 2015){
# Créer un histogramme du nombre de naissances par an d'un prénom
#
# Nécessite le jeu de données prenoms dans l'environnement global
  
  prenoms %>% 
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin)) %>% 
    group_by(annee) %>% 
    summarise(total = sum(nombre)) %>% 
    ggplot(aes(x = annee, y = total)) +
      geom_col(alpha = 0.7, fill = "tomato", color = "tomato2") +
      ggtitle(str_c("Nombre total en France")) +
      theme(axis.title = element_blank(),
            panel.background = element_blank())
}

creer_histogramme_prop <- function(Prenom, debut = 1900, fin = 2015){
  # Créer un histogramme des proportions de naissance par an d'un prénom
  #
  # Nécessite le jeu de données prenoms dans l'environnement global
  # Nécessite le jeu de données naissances dans l'environnement global
  naissances_filtre <- naissances %>% 
    filter(between(annee, debut, fin)) %>% 
    group_by(annee) %>% 
    summarise(naissances = sum(naissances))
  
  prenoms %>% 
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin)) %>% 
    group_by(annee) %>% 
    summarise(total = sum(nombre)) %>% 
    inner_join(naissances_filtre, by = "annee") %>% 
    mutate(prop = total / naissances * 100) %>% 
    ggplot(aes(x = annee, y = prop)) +
    geom_col(alpha = 0.7, fill = "tomato", color = "tomato2") +
    ggtitle(str_c("Proportion par an (%)")) +
    theme(axis.title = element_blank(),
          panel.background = element_blank())
}

creer_top_dept <- function(Prenom, debut = 1900, fin = 2015){
  # Créer un histogramme contenant les 5 premiers départements
  # avec le plus de ~Prenom entre ~debut et ~fin.
  # Nécessite le jeu de données prenoms dans l'environnement global
    prenoms %>%
    filter(prenom == str_to_upper(Prenom),
         between(annee, debut, fin)) %>% 
    group_by(code_insee) %>% 
    summarise(total = sum(nombre)) %>% 
    inner_join(france@data, by = "code_insee") %>% 
    mutate(depart = fct_reorder(nom_dept, total)) %>% 
    top_n(5, total) %>% 
    ggplot(aes(x = depart, y = total)) +
      geom_col(alpha = 0.7, fill = "coral", color = "coral2") +
      coord_flip() +
    ggtitle(str_c("Top 5 des départements\n(nombre de naissances)")) +
    theme(axis.title = element_blank(),
          panel.background = element_blank())
}  

creer_top_dept_prop <- function(Prenom, debut = 1900, fin = 2015){
  # Créer un histogramme contenant les 5 premiers départements
  # avec le plus de proportion de ~Prenom entre ~debut et ~fin.
  # Nécessite le jeu de données prenoms dans l'environnement global
  # Nécessite le jeu de données naissances dans l'environnement global
  
  calculer_prop(Prenom, debut, fin) %>% 
    inner_join(france@data, by = "code_insee") %>% 
    mutate(depart = fct_reorder(nom_dept, prop)) %>% 
    top_n(5, prop) %>% 
    ggplot(aes(x = depart, y = prop)) +
      geom_col(alpha = 0.7, fill = "coral", color = "coral2") +
      coord_flip() +
      ggtitle(str_c("Top 5 des départements\n(% des naissances)")) +
      theme(axis.title = element_blank(),
            panel.background = element_blank())
}  
