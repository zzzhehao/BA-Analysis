library(rstatix)
library(tidyverse)
library(MASS)
library(rlist)
library(ggpubr)

# Hatch rate data from CI-test from Jäckel, 2011, Tab. 35
hatchrate_wa2a2 <- c(0.8, 0.504, 0.763, 0.628, 0)*100 
hatchrate_wa2b <- c(0.333, 0.314, 0)*100
# Compute fitting normal distribution
fit.wa2a2.norm <- fitdistr(hatchrate_wa2a2, densfun = "normal")
fit.wa2b.norm <- fitdistr(hatchrate_wa2b, densfun = "normal")
# Compute non-parametric estimation
dens.wa2a2 <- density(hatchrate_wa2a2)
plot(dens.wa2a2)
trc.dens.wa2a2 <- data.frame(x = dens.wa2a2$x[dens.wa2a2$x>0 & dens.wa2a2$x<100], y = dens.wa2a2$y[dens.wa2a2$x>0 & dens.wa2a2$x<100]) # Truncate 0:1 range
plot(trc.dens.wa2a2) # w/ 0 or w/o 0?

dens.wa2b <- density(hatchrate_wa2b)
plot(dens.wa2b)
trc.dens.wa2b <- data.frame(x = dens.wa2b$x[dens.wa2b$x>0 & dens.wa2b$x<100], y = dens.wa2b$y[dens.wa2b$x>0 & dens.wa2b$x<100]) # Truncate 0:1 range
plot(trc.dens.wa2b)

#### Define functions ####
shapiro <- function(data) {
  shapirop <- data %>% group_by(CI) %>% shapiro_test(values) %>% dplyr::select(p) %>% t()
  return(shapirop)
}

difftest <- function(data) {
  colnames(data) <- c("CI", "values")
  norm_dis <- min(shapiro(data)) # Calculate shapiro p-value
  if (norm_dis > 0.05) {
    test <- data %>% ungroup() %>% t_test(values ~ CI) %>% dplyr::select(p)
  } else {
    test <- data %>% ungroup() %>% wilcox_test(values ~ CI) %>% dplyr::select(p)
  }
  return(test)
}

test_rep <- function(r) {
  # Resample data
  df <- as_tibble(data.frame(
    c(rep("wa2 x wa2",r),rep("wa2 x wb",r)), 
    # Bootstrap normal distribution
    c(
      rnorm(r, mean = fit.wa2a2.norm$estimate[1], sd = fit.wa2a2.norm$estimate[2])/100, # wa2 x wa2
      rnorm(r, mean = fit.wa2b.norm$estimate[1], sd = fit.wa2b.norm$estimate[2])/100 # wa2 x wb
    ),
    # Bootstrap Kernel Density Estimation
    c(
      sample(trc.dens.wa2a2$x, size = r, replace = TRUE, prob = trc.dens.wa2a2$y/sum(trc.dens.wa2a2$y))/100, # wa2 x wa2
      sample(trc.dens.wa2b$x, size = r, replace = TRUE, prob = trc.dens.wa2b$y/sum(trc.dens.wa2b$y))/100 # wa2 x wb
    )
  ))
  colnames(df) <- c("CI", "norm", "nonpar")

  test.matrix <- list(df[,c("CI", "norm")], df[,c("CI", "nonpar")]) # subset data, [[1]] norm, [[2]] nonpar
  
  wa2wb.nonpar <- df %>% filter(CI == "wa2 x wb") %>% dplyr::select(-norm) %>% mutate(value = nonpar)
  wa2n.wbpar <- df %>% filter(CI == "wa2 x wa2") %>% dplyr::select(-nonpar) %>% mutate(value = norm) %>% bind_rows(wa2wb.nonpar) %>% dplyr::select(c("CI", "value"))
  test.matrix <- list.append(test.matrix, wa2n.wbpar) # wa2 normal distribution vs. wb non-parametric estimation

  # Apply difftest()
  if (exists("pvalues") && is.data.frame(get("pvalues")) == TRUE) {
    pvalues <- bind_rows(pvalues,as_tibble(t(unlist(map(test.matrix, difftest)))))
  } else {
    pvalues <- as_tibble(t(unlist(map(test.matrix, difftest))))
  }
  
  return(pvalues)
}

bootstrap <- function(size) {
  # Calculate replicates
  if (exists("bstp") && is.data.frame(get("bstp")) == TRUE) {
    bstp <- bind_rows(bstp, t(replicate(iterations, test_rep(size), simplify = TRUE)))
  } else {
    bstp <- t(replicate(iterations, test_rep(size), simplify = TRUE))
  }
  return(bstp)
}

vis <- function (list, title) {
  Picasso = c("#5c363a", "#995041", "#0f6a81")
  # Visualization
  plot <- 
    as.data.frame(apply(list, 2, unlist)) %>%
    pivot_longer(1:3, names_to = "model", values_to = "pvalue") %>%
    mutate(model = case_when(
      model == "p" ~ "Normally distributed",
      model == "V2" ~ "Non-parametric",
      model == "V3" ~ "wA2 normally distributed \n wB non-parametric"
    )) %>%
    group_by(model) %>%
    ggplot()+
    geom_density(aes(x = pvalue, color = model)) +
    scale_x_log10(limits = c(1e-6,1)) +
    geom_vline(aes(xintercept = 0.05), color = "black") +
    theme_USGS_box() +
    theme(aspect.ratio = 0.4,
          legend.position = "none",
          strip.background = element_blank(),
          strip.text.y = element_blank()) +
    facet_grid(vars(model)) +
    scale_color_manual(values = Picasso) +
    xlab("") +
    ylab("") +
    labs(title = paste0("Sample size: ", title))
  return(plot)
}

#### Apply tests ####
size <- c(3,4,5,6,7,10,12,15,20,25) # Set simmulated sample sizes
iterations <- 1000 # Set bootstrap replicate amount

replicates <- map(size, bootstrap) # Calculation

plots <- map2(replicates, size, vis) # Visualization
rep <- ggarrange(plotlist = plots, nrow = 2, ncol = 5) # Plots arrangement
rep

ggsave(rep, file = "Plots/CI sample size bootstrap.png") # Save
