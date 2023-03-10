library(shiny)
library(shinyMatrix)

# Define UI
ui <- fluidPage(
  titlePanel("Grille de boutons cliquables"),
  
  sidebarLayout(
    sidebarPanel(),
    
    mainPanel(
      matrixInput("buttons", 
                  label = "Cliquez sur les boutons", 
                  value = matrix(rep("", 100), ncol = 10), 
                  rows = list(extend = FALSE), 
                  class = "numeric"
      )
    )
  )
)

# Define server
server <- function(input, output) {
  observe({
    for (i in 1:10) {
      for (j in 1:10) {
        id <- paste0("buttons-", i, "-", j)
        js <- sprintf("Shiny.onInputChange('%s', true)", id)
        tags$script(HTML(paste0("$('#", id, "').on('click', function() {", js, "})")))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

