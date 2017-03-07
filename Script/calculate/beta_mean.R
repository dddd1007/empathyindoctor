beta_mean <- function(cond_name = NA) {
  now_path <- getwd()
  choosepath <- tcltk::tk_choose.dir()
  setwd(choosepath)
  filepath <- dir(include.dirs = T, recursive = T)
  filepath <- filepath[stringr::str_detect(filepath, pattern = cond_name)]
  col_num <- length(extract_fNIRS_beta(filepath[1]))
  beta_table <- data.frame(matrix(0, nrow = length(filepath), ncol = col_num))
  names(beta_table) <- paste0("Ch", 1:col_num)

   for(i in filepath){
    beta_table[1:length(filepath), ] <- extract_fNIRS_beta(i)
  }

  beta_mean_result <- apply(beta_table, MARGIN = 2, mean)
  setwd(now_path)
  beta_list <- list(beta_mean = beta_mean_result, beta_total = beta_table)
}
