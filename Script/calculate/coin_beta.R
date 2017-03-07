coin_beta <- function(beta1, beta2) {
  beta1_total <- beta1$beta_total
  beta2_total <- beta2$beta_total

  ch_num <- ncol(beta1_total)
  ch_name <- names(beta1_total)

  beta1_total <- cbind(beta1_total, group = rep(1, nrow(beta1_total)))
  beta2_total <- cbind(beta2_total, group = rep(2, nrow(beta2_total)))
  beta <- rbind(beta1_total, beta2_total)

  result_coin <- list()
  for(i in 1: ch_num){
    result_coin[[i]] <- coin::oneway_test(beta[,i] ~ factor(group), data = beta)
  }

  return(result_coin)
}
