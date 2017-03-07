fnirs_heatmap <- function(filename, row = NULL, col = NULL) {
  map_row <- 2*row - 1
  map_col <- 2*col - 1
  dim_num <- map_row * map_col
  #map_vector <- read.csv(choose.file())
  map_table <- data.frame(matrix(0, nrow = dim_num, ncol = 4))
  names(map_table) <- c("X", "Y", "beta", "Channel")
  map_table$X <- rep(1:map_col, map_row)
  map_table$Y <- sort(rep(1 : map_row, map_col))
  ch <- 1:length(map_vector)

  for(i in 1:length(map_vector)){
    map_table[2*i,3] <- map_vector[i]
    map_table[2*i,4] <- ch[i]
  }
  map_table$beta[map_table$beta == 0] <- NA
  map_table$Channel[map_table$Channel == 0] <- NA
  map_table$beta <- Hmisc::impute(map_table$beta, mean)

  library(ggplot2)
  ggplot(map_table, aes(x = X, y = Y, fill = beta)) + geom_tile(show.legend = F) + scale_fill_continuous(high = "red", low = "blue") + geom_text(aes(label = Channel))+ coord_equal(ratio=1)
}
