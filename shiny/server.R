library(shiny)

shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_zem(input$leto1,input$vrsta1)
  })
})


narisi_zem = function(leto1, vrsta1){
  zemljevid = shrani.tabela2 %>%
    filter(vrsta1 != 'Stanovanja') %>%
    left_join(zemljevid, by = "statisticna_regija") %>%
    filter(leto == leto1, vrsta == vrsta1) %>%
    ggplot() +
    geom_polygon(
      mapping = aes(long, lat, group = group, fill = `Število`),
      color = "grey"
    ) +
    coord_map() +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.background = element_rect(colour = "grey", fill = "white")
    ) +
    labs(title = "Število gradbenih dovoljenj po vrsti stanovanj", size = 25)
  print(zemljevid)
}

