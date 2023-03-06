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
      nrow$val <- 15
      ncol$val <- 15
      nmine$val <- 30
    } else if (input$select_difficulty == 2) {
      nrow$val <- 20
      ncol$val <- 20
      nmine$val <- 100
    } else {
      nrow$val <- 26
      ncol$val <- 25
      nmine$val <- 250
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
    matrix
  }, colnames = FALSE)
  
}
# Define function to reveal cells and handle game logic
reveal_cell <- function(matrix, i, j) {
  
  # If the cell is already revealed or flagged, do nothing
  if (matrix[i, j] >= 0) {
    return(matrix)
  }
  
  # If the cell is a mine, reveal all mines and return -1
  if (matrix[i, j] == -1) {
    matrix[matrix == -1] <- 9
    return(matrix)
  }
  
  # Otherwise, reveal the cell
  matrix[i, j] <- abs(matrix[i, j])
  
  # If the revealed cell is a zero, reveal all adjacent cells recursively
  if (matrix[i, j] == 0) {
    for (ni in max(1, i - 1):min(nrow(matrix), i + 1)) {
      for (nj in max(1, j - 1):min(ncol(matrix), j + 1)) {
        if (matrix[ni, nj] < 0) {
          matrix <- reveal_cell(matrix, ni, nj)
        }
      }
    }
  }
  
  return(matrix)
}

# Run the application
shinyApp(ui = ui, server = server)