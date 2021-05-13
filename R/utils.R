reset_RDS_files <- function(init_val = NA){
  value_table <<- initialize_state_value(init_val)
  saveRDS(value_table, "value_table.RDS")
  
  stats_vector <- reactiveVal(c(0, 1))
  saveRDS(stats_vector, "stats_vector.RDS")
}


run_with_stats <- function(num_bins = 40, bin_size = 100, ...){
  
  for(i in seq_len(num_bins)){
    print(i)
    temp <- vector("double", bin_size)
    for(j in seq_len(bin_size))
      temp[[j]] <- game(...)
    
    stats_vector(append(stats_vector(), temp))
  }
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
