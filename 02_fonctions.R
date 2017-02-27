# Fonctions utilises pour recalculer les donnees et les traiter

library(sp)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Fonctions pour afficher la carte des prénoms ----------------------------

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
  
  data_prenom <- calculer_prop(Prenom, debut, fin)
  
  # S'il n'y a pas de données, afficher une carte blanche
  if(nrow(data_prenom) == 0){
    france_vide <- leaflet(france) %>% 
      addProviderTiles("CartoDB.Positron", 
                       options = providerTileOptions(maxZoom = 10, minZoom = 5)) %>% 
      addPolygons(color = "grey", weight = 1, fillColor = "white", opacity = 0.7) %>% 
      addPopups(2.213749, 46.22764, 
                "Pas de données trouvées pour le prénom saisi.",
                options = popupOptions(closeButton = FALSE))
    return(france_vide)
  }
  
  # Ajout des données de prénom à la carte
  france_prenom <- sp::merge(france, data_prenom)
  
  # Gestion du cas sans naissances
  france_prenom$prop[is.na(france_prenom$prop)] <- 0
  france_prenom$total[is.na(france_prenom$total)] <- 0
  
  # Fonction pour la palette de couleurs
  bpal <- colorBin("YlOrBr", france_prenom[[remplissage]], bins = 5)

  #Labels pour le passage de la souris
  labels <- paste0("<strong>", france_prenom$nom_dept, "</strong><br/>", 
                   round(france_prenom$total) ," naissances<br/>", 
                   formatC(france_prenom$prop, digits = 3), "% des naissances") %>% 
    lapply(htmltools::HTML)
  
  # La carte
  leaflet(france_prenom) %>% 
    addProviderTiles("CartoDB.Positron", 
                     options = providerTileOptions(maxZoom = 10, minZoom = 5)) %>% 
    addPolygons(color = "grey", weight = 1, 
                fillColor = bpal(france_prenom[[remplissage]]), fillOpacity = 0.7, 
                highlight = highlightOptions(
                  weight = 3,
                  color = "black",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
    addLegend("topleft", pal = bpal, values = france_prenom[[remplissage]],
              title = switch(remplissage, 
                             "prop" = "En %", 
                             "total" = "Nombre"), 
              opacity = 1,
              labFormat = labelFormat(big.mark = "")) %>% 
    clearBounds()
    
}

# Fonctions pour les graphes du panneau -----------------------------------

creer_histogramme <- function(Prenom, debut = 1900, fin = 2015){
# Créer un histogramme du nombre de naissances par an d'un prénom
#
# Nécessite le jeu de données prenoms dans l'environnement global
  
  donnees <- prenoms %>% 
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin)) %>% 
    group_by(annee) %>% 
    summarise(total = sum(nombre))
  
  if(nrow(donnees) == 0) return(NULL)
  
  ggplot(donnees, aes(x = annee, y = total)) +
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
  
  donnees <- prenoms %>% 
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin)) %>% 
    group_by(annee) %>% 
    summarise(total = sum(nombre)) %>% 
    inner_join(naissances_filtre, by = "annee") %>% 
    mutate(prop = total / naissances * 100)
  
  if(nrow(donnees) == 0) return(NULL)
  
  ggplot(donnees, aes(x = annee, y = prop)) +
    geom_col(alpha = 0.7, fill = "tomato", color = "tomato2") +
    ggtitle(str_c("Proportion par an (%)")) +
    theme(axis.title = element_blank(),
          panel.background = element_blank())
}

creer_top_dept <- function(Prenom, debut = 1900, fin = 2015){
  # Créer un histogramme contenant les 5 premiers départements
  # avec le plus de ~Prenom entre ~debut et ~fin.
  # Nécessite le jeu de données prenoms dans l'environnement global
  
  donnees <- prenoms %>%
    filter(prenom == str_to_upper(Prenom),
           between(annee, debut, fin))
  
  if(nrow(donnees) == 0) return(NULL)
  
  donnees <- donnees %>% 
    group_by(code_insee) %>% 
    summarise(total = sum(nombre)) %>% 
    inner_join(france@data, by = "code_insee") %>% 
    mutate(depart = fct_reorder(nom_dept, total)) %>% 
    top_n(5, total)
  
  ggplot(donnees, aes(x = depart, y = total)) +
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
  
  donnees <- calculer_prop(Prenom, debut, fin)
  
  if(nrow(donnees) == 0) return(NULL)
  
  donnees <- donnees %>% 
    inner_join(france@data, by = "code_insee") %>% 
    mutate(depart = fct_reorder(nom_dept, prop)) %>% 
    top_n(5, prop)
  
  ggplot(donnees, aes(x = depart, y = prop)) +
    geom_col(alpha = 0.7, fill = "coral", color = "coral2") +
    coord_flip() +
    ggtitle(str_c("Top 5 des départements\n(% des naissances)")) +
    theme(axis.title = element_blank(),
          panel.background = element_blank())
}  
