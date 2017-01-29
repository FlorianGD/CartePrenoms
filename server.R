source("02_fonctions.R")

function(input, output, session) { 

  output$map <- renderLeaflet({

    carte <- tmap_leaflet(creer_carte(input$prenom, input$dates[1], input$dates[2]))
    carte
    })
  
  output$histogramme <- renderPlot({
    creer_histogramme(input$prenom, input$dates[1], input$dates[2])
  })
  
  output$top_dep <- renderPlot({
    creer_top_dept(input$prenom, input$dates[1], input$dates[2])
  })

}