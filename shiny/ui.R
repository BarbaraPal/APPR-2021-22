library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarPanel(position = "left",
                sidebarPanel(
                  width = 8,
                  selectInput(
                    "leto1",
                    label = "Leto:",
                    choices = c(2010:2020),
                    selected = 2010
                  ),
                  selectInput(
                    "vrsta1",
                    label = "Vrsta stanovanja:",
                    choices = c("Enosobna", 
                                "Dvosobna",
                                "Trisobna", 
                                "Štirisobna", 
                                "Pet ali večsobna")),
                    selected = "Enosobna"
                  ))
                ,
  mainPanel(plotOutput("graf")))
  )