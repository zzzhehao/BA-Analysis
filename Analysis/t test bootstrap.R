library(rstatix)
library(tidyverse)

# wa2xwa2 0.5:0.8, wa2xwb 0:0.4
ttest_rep <- function(r) {
  data <- data.frame(c(rep("wa2 x wa2",r),rep("wa2 x wb",r)), c(sample(40:90, r, replace = TRUE), sample(0:50, r, replace = TRUE)))
  colnames(data) <- c("CI", "hatchrate")
  data$hatchrate <-  data$hatchrate / 100
  
  ttest <- data %>% ungroup() %>% t_test(hatchrate ~ CI) %>% select(p)
  if (exists("stat.result") && is.data.frame(get("stat.result")) == TRUE) {
    stat.result <- add_row(stat.result, ttest)
  } else {
    stat.result <- ttest
  }
  return(as.double(stat.result))
}

ttest_bootstrap <- data.frame(value = replicate(1000, ttest_rep(3)), samp = 3)
ttest_bootstrap <- add_row(ttest_bootstrap, data.frame(value = replicate(1000, ttest_rep(4)), samp = 4))
ttest_bootstrap <- add_row(ttest_bootstrap, data.frame(value = replicate(1000, ttest_rep(5)), samp = 5))
ttest_bootstrap <- add_row(ttest_bootstrap, data.frame(value = replicate(1000, ttest_rep(10)), samp = 10))
ttest_bootstrap$samp <- as.factor(ttest_bootstrap$samp)

ttest_bootstrap %>% group_by(samp) %>%
  ggplot()+
  geom_density(aes(x = value)) +
  scale_x_log10() +
  facet_grid(vars(samp)) +
  geom_vline(aes(xintercept = 0.05), color = "red")