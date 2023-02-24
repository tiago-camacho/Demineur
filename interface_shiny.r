library(shiny)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("My Shiny App"),
  
  # Create a table
  tableOutput("mytable")
)

# Define server logic
server <- function(input, output) {
  # Create a data frame for the table
  mydata <- data.frame(
    final_grid
  )
  
  # Display the table in the UI
  output$mytable <- renderTable({
    mydata
  })
}

# Run the application
shinyApp(ui = ui, server = server)
