library(leaflet)

fluidPage(
  
  # Attention si pas de taille définie, height ne peut pas être précisé en relatif
  # sidebarLayout(
  #   sidebarPanel(textInput("prenom", "Prénom", "Florian"),
  #                sliderInput("dates", "Dates", 1900, 2015,
  #                            value = c(1900, 2015), step = 1, sep ="")),
  #                mainPanel(leafletOutput("map", width="100%"))
  # )
  div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),
      
      leafletOutput("map", width="100%", height="100%"),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Nombre de naissances"),
                    
                    textInput("prenom", "Prénom", "Florian"),
                    sliderInput("dates", "Dates", 1900, 2015,
                                value = c(1900, 2015), step = 1, sep =""),
                    
                    plotOutput("histogramme", height = 200)
      )
)
)
