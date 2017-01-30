source("02_fonctions.R")

function(input, output, session) { 

  output$map <- renderLeaflet({

    carte <- tmap_leaflet(
      creer_carte(input$prenom, input$dates[1], input$dates[2],
                  remplissage = input$choix)
      )
    carte
    })
  
  output$histogramme <- renderPlot({
    switch(input$choix,
           prop = creer_histogramme_prop(input$prenom, input$dates[1], input$dates[2]),
           total = creer_histogramme(input$prenom, input$dates[1], input$dates[2]))
  })
  
  output$top_dep <- renderPlot({
    switch(input$choix,
           prop = creer_top_dept_prop(input$prenom, input$dates[1], input$dates[2]),
           total = creer_top_dept(input$prenom, input$dates[1], input$dates[2]))
  })

}