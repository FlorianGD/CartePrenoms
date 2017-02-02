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
                                 big.mark = "",
                                 decimal.mark = ",")) +
    tm_view(set.zoom.limits = c(5, 9), 
            legend.position = c("left", "top"))
}

ameliorer_popup <- function(carte){
  # Améliore les popup. Pour l'instant, plutôt un "hack", d'autres
  # fonctionnalités de tmap devraient venir
  leafmap <- tmap_leaflet(carte)
  
  leafmap$x$calls[[4]]$args[[5]] <- leaflet:::evalFormula(
    ~paste0(
      "<div style=\"max-height:10em;overflow:auto;\"><table>\n
      \t\t\t   <thead><tr><th colspan=\"2\"><b>", nom_dept, "</b></th></thead></tr>
      <tr><td style=\"color: #888888;\"> Total :&nbsp; </td><td>", round(total), "</td></tr>
      <tr><td style=\"color: #888888;\"> Proportion :&nbsp; </td><td>", 
      formatC(prop, digits = 3, decimal.mark = ",") ,"</td></tr>
      </table></div>"
    ),
    data=carte$tm_shape$shp@data
    )
  
  leafmap
}
#### Fonctions pour les graphes du paneau
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
