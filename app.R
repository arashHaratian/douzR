library(shiny)
source("ttt.R")

ui <- fluidPage(

  titlePanel(h1("fuel my agent!", align = "center"), "douzR"),
  fluidRow(h2(textOutput("precision")), align = "center"),
  fluidRow(plotOutput("board")),
  fluidRow(plotOutput("stats"))
)

server <- function(input, output, session) {

  
  # precision section
  output$precision <- renderText({
    paste("douzR precision: ", sum(is.na(value_table)), "%") #TODO: getting percision
    })
  
  # board section
  
  
  
  
  # saving `value_table`
  onStop(function() {
    saveRDS(value_table, "value_table.RDS")
  })
}

shinyApp(ui, server)
