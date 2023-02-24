library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Démineur"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("btn_difficulty", "Choisir la difficulté")
    ),
    
    mainPanel(
      h3("Grille de démineur"),
      tableOutput("grid")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Define reactive values for difficulty level
  nrow <- reactiveValues(val = 10)
  ncol <- reactiveValues(val = 10)
  nmine <- reactiveValues(val = 10)
  
  # Open dialog box for difficulty level selection
  observeEvent(input$btn_difficulty, {
    showModal(
      modalDialog(
        title = "Choisir la difficulté",
        selectInput("select_difficulty", "Niveau de difficulté",
                    choices = c("Facile" = 1, "Moyen" = 2, "Difficile" = 3)),
        footer = modalButton("Valider")
      )
    )
  })
  
  # Update reactive values based on difficulty level selection
  observeEvent(input$select_difficulty, {
    if (input$select_difficulty == 1) {
      nrow$val <- 10
      ncol$val <- 10
      nmine$val <- 10
    } else if (input$select_difficulty == 2) {
      nrow$val <- 20
      ncol$val <- 20
      nmine$val <- 40
    } else {
      nrow$val <- 40
      ncol$val <- 40
      nmine$val <- 80
    }
    removeModal()
  })
  
  # Generate matrix and grid based on reactive values
  output$grid <- renderTable({
    matrix <- matrix(0, nrow = nrow$val, ncol = ncol$val)
    mine_locs <- sample(nrow$val * ncol$val, nmine$val)
    matrix[mine_locs] <- -1
    for (i in 1:nrow$val) {
      for (j in 1:ncol$val) {
        if (matrix[i, j] != -1) {
          matrix[i, j] <- sum(matrix[max(1,i-1):min(nrow$val,i+1),max(1,j-1):min(ncol$val,j+1)] == -1)
        }
      }
    }
    colnames(matrix) <- NULL
    matrix
  })
}
# Run the application
shinyApp(ui = ui, server = server)
