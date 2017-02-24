fluidPage(title = "Carte des prénoms en France",
          
          div(class="outer",
              
              tags$head(
                # Include our custom CSS
                includeCSS("styles.css")
              ),
              
              leafletOutput("map", width="100%", height="100%"),
              
              # Shiny versions prior to 0.11 should use class="modal" instead.
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 0, left = "auto", right = 0, bottom = "auto",
                            width = 330, height = "auto",
                            
                            h2("Naissances"),
                            
                            textInput("prenom", "Prénom", "Florian"),
                            sliderInput("dates", "Dates", 1900, 2015,
                                        value = c(1900, 2015), step = 1, sep =""),
                            radioButtons("choix", NULL, 
                                         c("Proportion de naissances" = "prop",
                                           "Nombre de naissances" = "total")),
                            
                            plotOutput("histogramme", height = 200),
                            
                            plotOutput("top_dep", height = 200)
              )
          ),
          
          tags$div(id="cite",
                   "Source : ", 
                   tags$a(href = "https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/",
                          target="_blank",
                          "OpenStreetMap"),
                   " et", 
                   tags$a(href = "http://www.data.gouv.fr/fr/datasets/fichier-des-prenoms-edition-2016/", 
                          target="_blank",
                          "INSEE"),
                   ". Réalisé par Florian Gaudin-Delrieu. ", 
                   tags$a(href = "https://github.com/ptiflus/CartePrenoms", 
                          target="_blank",
                          "Code source")
                   )
)

