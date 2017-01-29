library(leaflet)

fluidPage(

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
                    
                    h2("Naissances"),
                    
                    textInput("prenom", "Prénom", "Florian"),
                    sliderInput("dates", "Dates", 1900, 2015,
                                value = c(1900, 2015), step = 1, sep =""),
                    
                    plotOutput("histogramme", height = 200),
                    
                    plotOutput("top_dep", height = 200)
      )
)
)
