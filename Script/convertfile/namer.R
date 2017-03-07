for(i in a){
  table <- read.table(i, sep = ",")[1:32,]
  table <- cbind(stringr::str_c(c("S","D") , rep(1:16, rep(2,16)), sep = ""), table)
  colnames(table) <- c("Optode (MNI)","X", "Y", "Z")
  filename <- stringr::str_c(str_sub(i, end = -5), "convert.csv", sep = "_")
  write.csv(table, filename, col.names = TRUE, row.names = FALSE)
}
