library(leaflet)

fluidPage(
  
  # Attention si pas de taille définie, height ne peut pas être précisé en relatif
  sidebarLayout(
    sidebarPanel(textInput("prenom", "Prénom", "Florian"),
                 sliderInput("dates", "Dates", 1900, 2015,
                             value = c(1900, 2015), step = 1, sep ="")),
                 mainPanel(leafletOutput("map", width="100%"))
  )
)
