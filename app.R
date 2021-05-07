library(shiny)
source("ttt.R")

ui <- fluidPage(
  titlePanel(h1("fuel my agent!", align = "center"), "douzR"),
  fluidRow(h2(textOutput("precision")), align = "center"),
  fluidRow(column(8,
                  plotOutput(
                    "board_plot",
                    click = "player_two_move",
                    width = "auto",
                    height = 700),
                  offset = 2)),
  fluidRow(plotOutput("stats_plot")),
  tableOutput("data")
)




server <- function(input, output, session) {
  
  # parameters ----------
  eps <- 0.01
  alpha <- 0.5
  old_state_index <- reactiveVal()
  winner <- reactiveVal()
  
  
  
  # precision section ---------
  output$precision <- renderText({
    paste("douzR precision: ",sum(!is.na(value_table)) ,"%") #TODO: getting percision
  })
  
  # board section ---------
  board <- initialize()
  board_reactive <- reactiveVal(board)
  
  melt_board_reactive <- reactive({
    melt(board_reactive()) %>%
      mutate(Var1 = factor(Var1),
             Var2 = factor(Var2, levels = c(3, 2, 1)))
  })
  
  
  player_two_move_reactive <- eventReactive(
    input$player_two_move, {
      point <- nearPoints(
        melt_board_reactive(),
        input$player_two_move,
        "Var1",
        "Var2",
        threshold = 100,
        maxpoints = 1)
      
      if(nrow(point) != 0){
        point <- point[1:2]
        switch (as.character(point[[1]]),
                "1" = index <- as.integer(levels( point[[2]] ) [[ point[[2]] ]] ),
                "2" = index <- 3L + as.integer(levels( point[[2]] ) [[ point[[2]] ]] ),
                "3" = index <- 6L + as.integer(levels( point[[2]] ) [[ point[[2]] ]] )
        )
        
      } else{
        index <- 0
      }
      index
    }
  )
  
  
  
  # player move
  observeEvent(input$player_two_move, {
    # browser()
    print("player")
    old_state_index(state_index(board))
    
    player_two_move <- player_two_move_reactive()
    while (!(player_two_move %in% possible_moves(board)))
      player_two_move <- player_two_move_reactive()
    
    board <<- next_state(board, player = player_two, move = player_two_move) #player_two(O) move
    board_reactive(board)
    
    if (is_done(board)) {
      current_state_index <- state_index(board)
      value_table <<- value_update(old_state_index(), current_state_index, alpha)
      winner(value_get(value_table, current_state_index))
      old_state_index(NULL)
    }
  })
  
  
  # RL move
  observeEvent(old_state_index(), {
    print("RL")
    req(old_state_index())

    player_one_move <- next_move(board, player = player_one, epsilon = eps)
    
    board <<- next_state(board, player = player_one, move = player_one_move$move)
    board_reactive(board)
    
    if(player_one_move$is_greedy){
      current_state <- state_index(board)
      value_table <<- value_update(old_state_index(), current_state, alpha)
    }
    
    if (is_done(board)) {
      index <- state_index(board)
      winner(value_get(value_table, index))
    }
  })
  
  
  
  
  output$board_plot <- renderPlot({
    symbols <- setNames(c("O", " ", "X"), c(-1, 0, 1))
    ggplot(melt_board_reactive(), aes(Var2, Var1)) +
      geom_tile(color = "black", fill = "white") +
      geom_text(aes(label = symbols[as.character(value)]), size = 20) +
      labs(x = "", y = "", color = "")
  })
  
  
  
  output$data <- renderTable({
    print(winner())
  })
  
  # saving `value_table` ---------
  onStop(function() {
    saveRDS(value_table, "value_table.RDS")
  })
}

shinyApp(ui, server)
