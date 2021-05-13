library(plotly)
library(shinythemes)

fluidPage(
  style = "background-color: #bad7df; color: #0061a8;",
  theme = shinytheme("flatly"),
  
  titlePanel(h1("Welcome to DouzR", align = "center"), "douzR"),
  
  h3("FUEL MY AGENT !", align = "center"),
  h4("Play some tic-tac-toe and let my agent learn from you.", align = "center"),
  
  fluidRow(br(), h2(textOutput("precision")), align = "center"),
  
  fluidRow(br(), column(
    8,
    plotOutput(
      "board_plot",
      click = "player_two_move",
      width = "auto",
      height = 700
    ),
    offset = 2
  )),
  
  fluidRow(br(), uiOutput("play_again_button"), align = "center"),
  
  fluidRow(br(), h2("Stats"), align = "center"),
  
  h4(
    "The plot below shows the win rate of the douzR against other players.
     Each point is an average of ",
    textOutput("number_of_games", inline = TRUE),
    "game results."
  ),
  
  
  fluidRow(
    selectInput(
      "update_stats_plot",
      "Choose the number of game results to calculate the average:",
      choices = c(1, 10, 50, 100, 1000),
      selected = 50
    ),
    align = "center"
  ),
  
  fluidRow(br(), column(
    8,
    plotlyOutput("stats_plot", width = "auto", height = 500),
    offset = 2
  )),
  
  fluidRow(br(), h2("About"), align = "center"),
  
  h4(
    "This game is a re-implementation of the tic-tac-toe game from the first chapter of Reinforcement Learning: An Introduction, 2nd edition by Richard Sutton and Andrew Barto.
  douzR uses a Reinforcement learning algorithm called temporal difference learning (TD).
  This method interacts with the environment and updates the state values."
  ),
  h4(" Learn more:"),
  tags$div(tags$ul(
    tags$li(
      tags$a(href = "http://incompleteideas.net/book/the-book.html", "Reinforcement Learning: An Introduction")
    ),
    tags$li(
      tags$a(href = "http://incompleteideas.net/book/code/TTT.lisp", "Original code for tic-tac-toe with TD method")
    ),
    tags$li(
      tags$a(
        href = "https://github.com/arashHaratian/Reinforcement_Learning-an_Introduction-in_R",
        "R re-implementation of Reinforcement Learning: an Introduction"
      )
    )
  )) #,  style = "font-size: 15px"
)