library(shiny)

# Fonction pour générer un champ de mines aléatoire
generate_board <- function(width, height, num_mines) {
  board <- matrix(0, nrow = height, ncol = width)
  mines <- sample(1:(width * height), num_mines)
  board[mines] <- -1
  board[board != -1] <- sapply(1:(width * height - num_mines), function(x) {
    adjacent_mines <- sum(board[get_neighbors(x, width, height)] == -1)
    if (adjacent_mines > 0) {
      return(adjacent_mines)
    } else {
      return(0)
    }
  })
  return(board)
}

# Fonction pour récupérer les cases voisines d'une case donnée
get_neighbors <- function(index, width, height) {
  neighbors <- c()
  if ((index - width - 1) %% width != 0 && index > width) {
    neighbors <- c(neighbors, index - width - 1)
  }
  if (index %% width != 0 && index > width) {
    neighbors <- c(neighbors, index - width)
  }
  if ((index + 1) %% width != 0 && index < width * (height - 1)) {
    neighbors <- c(neighbors, index + 1)
  }
  if ((index + width) %% width != 0 && index < width * (height - 1)) {
    neighbors <- c(neighbors, index + width)
  }
  if ((index + width + 1) %% width != 0 && index < width * (height - 1)) {
    neighbors <- c(neighbors, index + width + 1)
  }
  if ((index - width + 1) %% width != 0 && index > width) {
    neighbors <- c(neighbors, index - width + 1)
  }
  if (index %% width != 0 && index < width * (height - 1)) {
    neighbors <- c(neighbors, index + width)
  }
  if ((index - 1) %% width != 0 && index > width) {
    neighbors <- c(neighbors, index - 1)
  }
  return(neighbors)
}

# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Démineur"),
  sidebarLayout(
    sidebarPanel(
      numericInput("width", "Largeur du champ de mines :", 10),
      numericInput("height", "Hauteur du champ de mines :", 10),
      numericInput("num_mines", "Nombre de mines :", 10),
      actionButton("reset_button", "Nouvelle partie")
    ),
    mainPanel(
      plotOutput("board_plot")
    )
  )
)

# Serveur Shiny
server <- function(input, output, session) {
  board <- reactive({
    generate_board(input$width, input$height, input$num_mines)
  })
  
  # Fonction pour révéler une case
  reveal_cell <- function(x, y) {
    if (board()[y, x] == -1) {
      board()[y, x] <<- -2
      for (i in 1:input$height) {
        for (j in 1:input$width) {
          if (board()[i, j] == -1) {
            board()[i, j] <<- -2
          }
        }
      }
      show_modal("Démineur", "BOOM ! Vous avez perdu. Cliquez sur Nouvelle partie pour jouer à nouveau.")
    } else if (board()[y, x] == 0) {
      board()[y, x] <<- -3
      neighbors <- get_neighbors((y - 1) * input$width + x, input$width, input$height)
      for (neighbor in neighbors) {
        reveal_cell((neighbor - 1) %% input$width + 1, floor((neighbor - 1) / input$width) + 1)
      }
    } else {
      board()[y, x] <<- -board()[y, x]
    }
  }
  
  #Fonction pour marquer une case avec un drapeau
  
  flag_cell <- function(x, y) {
    if (board()[y, x] > 0) {
      board()[y, x] <<- -board()[y, x]
    }
  }
  
  #Fonction pour afficher une fenêtre modale avec un message
  
  show_modal <- function(title, message) {
    tags$script("
$(document).ready(function() {
$('#modal').modal('show');
});
")
    modalDialog(
      title = title,
      message = message,
      easyClose = TRUE
    )
  }
  
  #Fonction pour afficher le champ de mines
  
  output$board_plot <- renderPlot({
    plot.new()
    plot.window(xlim = c(0, input$width), ylim = c(0, input$height))
    for (i in 1:input$height) {
      for (j in 1:input$width) {
        if (board()[i, j] == 0) {
          text(j - 0.5, input$height - i + 0.5, "", cex = 3, col = "gray")
        } else if (board()[i, j] == -1) {
          text(j - 0.5, input$height - i + 0.5, "", cex = 3, col = "red")
        } else if (board()[i, j] == -2) {
          text(j - 0.5, input$height - i + 0.5, "", cex = 3, col = "black")
        } else if (board()[i, j] == -3) {
          text(j - 0.5, input$height - i + 0.5, "", cex = 3, col = "gray")
        } else {
          text(j - 0.5, input$height - i + 0.5, board()[i, j], cex = 3, col = "black")
        }
      }
    }
  })
  
  #Fonction pour réinitialiser le champ de mines
  
  observeEvent(input$reset_button, {
    board(generate_board(input$width, input$height, input$num_mines))
  })
  
  #Événements de clic pour révéler ou marquer une case
  
  observe({
    for (i in 1:input$height) {
      for (j in 1:input$width) {
        id <- paste0("cell_", i, "", j)
        if (!input[[id]]$clicked) {
          observeEvent(input[[id]], {
            if (input$reset_button == 0) {
              reveal_cell(j, i)
            } else {
              input[[id]] <- list(clicked = FALSE)
            }
          })
        }
        id <- paste0("flag", i, "_", j)
        if (!input[[id]]$clicked) {
          observeEvent(input[[id]], {
            if (input$reset_button == 0) {
              flag_cell(j, i)
            } else {
              input[[id]] <- list(clicked = FALSE)
            }
          })
        }
      }
    }
  })
}

#Application Shiny

shinyApp(ui = ui, server = server)
