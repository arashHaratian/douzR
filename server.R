function(input, output, session) {
  
  
  
  # parameters ----------
  eps <- 0.01
  alpha <- 0.5
  old_state_index <- reactiveVal()
  last_state_value <- reactiveVal()   #TODO
  winner <- reactiveVal()
  update_stats_plot <- 100    #TODO
  value_table <<- readRDS("value_table.RDS")
  stats_list <<- readRDS("stats_list.RDS")
  
  
  # precision section ---------
  output$precision <- renderText({
    paste("douzR precision: ", round(mean(stats_list$averages) * 100, 2)  ,"%") #TODO: getting percision
  })
  
  # board section ---------
  board <- initialize()
  board_reactive <- reactiveVal(board)
  
  melt_board_reactive <- reactive({
    melt(board_reactive()) %>%
      mutate(
        Var1 = factor(Var1),
        Var2 = factor(Var2, levels = c(3, 2, 1)),
        labels = if_else(value == 1 , "X", if_else(value == -1, "O", ""))
      )
  })
  
  
  output$board_plot <- renderPlot({
    ggplot(melt_board_reactive(), aes(Var2, Var1)) +
      geom_tile(color = "#45eba5", fill = "#374785") +
      geom_text(aes(label = labels, color = if_else(labels == "O" , "col1", "col2")), size = 25) +
      win_position(board_reactive()) +
      theme_void() +
      theme(legend.position = "none", plot.background = element_rect(fill = "#393e46"))
  })
  
  
  # players section ---------
  
  player_two_move_reactive <- eventReactive(
    input$player_two_move, {
      point <- nearPoints(
        melt_board_reactive(),
        input$player_two_move,
        "Var1",
        "Var2",
        threshold = 110,
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
    print("player")
    
    player_two_move <- player_two_move_reactive()
    if(!(player_two_move %in% possible_moves(board)) | is_done(board))
      return()
    
    old_state_index(state_index(board))
    
    board <<- next_state(board, player = player_two, move = player_two_move) #player_two(O) move
    board_reactive(board)
    
    if (is_done(board)) {
      current_state_index <- state_index(board)
      value_table <<- value_update(old_state_index(), current_state_index, alpha)
      last_state_value(value_get(value_table, current_state_index))
      winner(whos_winner(board))
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
      last_state_value(value_get(value_table, index))
      winner(whos_winner(board))
    }
  })
  
  
  
  
  # end section ---------
  
  # show notification and render button
  observeEvent(winner(), {
    
    message <- switch(as.character(winner()),
                      "1" = "douzR is the winner.",
                      "-1" =  "You are the winner.",
                      "TIE!")
    
    output$play_again_button <-
      renderUI(actionButton(
        "play_again",
        label = div(h3("Play Again", icon("undo"))),
        width = "25%",
        style = 'padding:20px',
        class = "btn-primary"
      ))
    
    showNotification(message,
                     id = "notification",
                     duration = NULL)
    
    stats_list$winners <- append(stats_list$winners, last_state_value())
  })
  
  
  observeEvent(input$play_again, {
    old_state_index(NULL)
    last_state_value(NULL)
    winner(NULL)
    board <<- initialize()
    board_reactive(board)
    saveRDS(value_table, "value_table.RDS")    #TODO
    removeUI(selector = '#play_again', immediate = TRUE)
    removeNotification("notification")
  })
  
  # stats plot section -------
  
  observeEvent(input$player_two_move, { #TODO
    if(F)
      run_with_stats(40, bin_size = update_stats_plot)
  })
  
  
  observe({
    if(length(stats_list$winners) == update_stats_plot){
      stats_list$averages <- append(stats_list$averages, mean(stats_list$winners))
      stats_list$winners <- c()
    }
  })
  
  
  
  output$stats_plot <- renderPlotly({
    df <- data.frame("averages" = stats_list$averages) %>% 
      mutate(x_axis = seq_len(length(averages)))
    
    thematic::thematic_local_theme(thematic::thematic_theme(bg = "#393e46", fg = "#00adb5", font = "#374785"))
    
    plot <- ggplot(df, aes(x = x_axis, y = averages, color = "col")) +
      geom_line() +
      geom_point() +
      theme(legend.position = "none") +
      labs(x = "",
           y = "Performance")
    
    
    ggplotly(plot)
    
  })
  
  output$number_of_games <- renderText(update_stats_plot)
  
  
  # log section --------
  observe({
    cat("\n\n")
    cat("last_state_value: ", last_state_value(), "\n")
    cat("winner: ", winner(), "\n")
    cat("old_state_index: ", old_state_index(), "\n")
    cat("board_reactive", board_reactive(), "\n")
    cat("statslist", stats_list$winners, "\n")
    cat("statslist", stats_list$averages, "\n")
    cat("\n\n")
    
  })
  
  # saving `value_table` and `stats_list` ---------
  onStop(function() {
    message("session ended")
    saveRDS(value_table, "value_table.RDS")
    saveRDS(stats_list, "stats_list.RDS")
  })
  
  
}