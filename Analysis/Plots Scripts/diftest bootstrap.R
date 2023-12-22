library(rstatix)
library(tidyverse)

# wa2xwa2 0.5:0.8, wa2xwb 0:0.4
test_rep <- function(r) {
  data <- data.frame(c(rep("wa2 x wa2",r),rep("wa2 x wb",r)), c(sample(0:90, r, replace = TRUE), sample(0:50, r, replace = TRUE)))
  colnames(data) <- c("CI", "hatchrate")
  data$hatchrate <-  data$hatchrate / 100
  
  norm_dis <- shapiro_test(data$hatchrate)$`p.value`
  
  if (norm_dis > 0.05) {
    test <- data %>% ungroup() %>% t_test(hatchrate ~ CI) %>% select(p)
  } else {
    test <- data %>% ungroup() %>% wilcox_test(hatchrate ~ CI) %>% select(p)
  }
  
  if (exists("stat.result") && is.data.frame(get("stat.result")) == TRUE) {
    test_rep_res <- add_row(stat.result, test)
  } else {
    test_rep_res <- test
  }
  return(as.double(test_rep_res))
}

test_bootstrap <- data.frame(value = replicate(1000, test_rep(3)), samp = 3)
test_bootstrap <- add_row(test_bootstrap, data.frame(value = replicate(1000, test_rep(4)), samp = 4))
test_bootstrap <- add_row(test_bootstrap, data.frame(value = replicate(1000, test_rep(5)), samp = 5))
test_bootstrap <- add_row(test_bootstrap, data.frame(value = replicate(1000, test_rep(10)), samp = 10))
test_bootstrap$samp <- as.factor(test_bootstrap$samp)

pvalue_dens <- test_bootstrap %>% group_by(samp) %>%
  ggplot()+
  geom_density(aes(x = value)) +
  scale_x_log10() +
  facet_grid(vars(samp)) +
  geom_vline(aes(xintercept = 0.05), color = "red") +
  theme_USGS_box() +
  theme(aspect.ratio = 0.3) +
  xlab("p-value") +
  ylab("p-value Density")
pvalue_dens

ggsave(pvalue_dens, file = "Plots/pvalue_dens.png")