reset_value_table <- function(init_val = NA){
  value_table <<- initialize_state_value(init_val)
  saveRDS(value_table, "value_table.RDS")
  
  stats_list <- reactiveValues("winners" = c(), "averages" = c(0.5)) # , "total_average" = 0.5, "total_count" = 0
  saveRDS(stats_list, "stats_list.RDS")
}


run_with_stats <- function(num_bins = 40, bin_size = 100, ...){
  result <- run(num_bins, bin_size, ...)
  stats_list$averages <- append(stats_list$averages, result)
}


whos_winner <- function(board) {
  if (any(colSums(board) == 3L) | any(rowSums(board) == 3L) |
      any(sum(diag(board)) == 3L) |
      any(sum(diag(board[nrow(board):1, ])) == 3L)) {
    return(1) ## for player one
  }
  if (any(colSums(board) == -3L) | any(rowSums(board) == -3L) |
      any(sum(diag(board)) == -3L) |
      any(sum(diag(board[nrow(board):1, ])) == -3L)) {
    return(-1)  ## for player two
  } else if (all(board != 0)) {
    return(0)  ## tie
  }
}


win_position <- function(board){
  
  temp <- colSums(board) == 3L | colSums(board) == -3L
  if(any(temp)){
    x_pos <- which(rev(temp)) # reverse of `temp` because x axis is invert
    return(geom_segment(aes(x = x_pos, xend = x_pos, y = 0.7, yend = 3.3), size = 3)) 
  }
  temp <- rowSums(board) == 3L | rowSums(board) == -3L
  if(any(temp)){
    y_pos <- which(temp)
    return(geom_segment(aes(x = 0.7, xend = 3.3, y = y_pos, yend = y_pos), size = 3))
  }
  if(sum(diag(board)) == -3L |  sum(diag(board)) == 3L){
    min_pos <- 0.7
    max_pos <- 3.3
    return((geom_segment(aes(x = max_pos, xend = min_pos,    # because x_axis is inverted
                             y = min_pos, yend = max_pos), size = 3)))  
  }
  
  if(sum(diag(board[nrow(board):1, ])) == -3L |  sum(diag(board[nrow(board):1, ])) == 3L){
    min_pos <- 0.7
    max_pos <- 3.3
    return((geom_segment(aes(x = min_pos, xend = max_pos, y = min_pos, yend = max_pos), size = 3)))  
  }
}
