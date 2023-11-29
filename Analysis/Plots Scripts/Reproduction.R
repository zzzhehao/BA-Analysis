library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(purrr)

# Import Data
raw <- read_xlsx("/Users/hu_zhehao/Library/Mobile Documents/com~apple~CloudDocs/UHH/B.Sc. Biologie/Bachelorarbeit/DNA Samples/Zhehao_Hu_Bachelorthesis_Data.xlsx", sheet = "Reproduction", range = "A3:P61") %>% filter(!is.na(Group))
colnames(raw)[5:16]  <- c("sum_layed","sum_hatched", "sum_grown", "6/27_layed", "6/27_hatched", "6/27_grown","7/4_layed", "7/4_hatched", "7/4_grown","7/11_layed", "7/11_hatched", "7/11_grown")

# calculate hatch rate
raw <- raw %>% mutate(sum_hatch_rate = sum_hatched / sum_layed)

library(ggplot2)
# hatch rate
n_fun.p1 <- function(x){
  return(data.frame(y = 1.15,
                    label = length(x)))
}
n.hatchrate <- c(
  'HT1' = "HT1",
  'HT1st' = "HT1*",
  'HT2/2st' = "HT2/2*"
)

#### Tests ####
library(rstatix)
library(tidyverse)
library(broom)

rp.test <- raw

shapiro <- rp.test %>% 
  filter(!is.na(Haplotype)) %>%
  group_by(Group, Haplotype) %>%
  nest() %>%
  mutate(Shapiro = map(data, ~shapiro.test(.x$sum_hatch_rate)))
shapiro_g <- shapiro %>% 
  mutate(shapiro_g = Shapiro %>% map(glance)) %>%
  unnest(shapiro_g)
shapiro_g

stat.result <- rp.test %>% ungroup() %>% filter(!is.na(sum_hatch_rate)) %>% group_by(Haplotype) %>% t_test(sum_hatch_rate ~ Group) %>% add_significance()
stat.result

stat.result <- stat.result %>% add_xy_position(x = "Group", dodge = 0.8)
stat.result <- mutate(stat.result, y.position = 1.1)

#### hatchrate ####
n_fun.hr <- function(x){
  return(data.frame(y = 1.2,
                    label = length(x)))
}
pplot.hatchrate <-
  ggplot(raw,aes(x=Group,y=sum_hatch_rate)) + 
  geom_boxplot(aes(fill=Group)) + 
  facet_grid(.~Haplotype,labeller=as_labeller(n.hatchrate),scales='free_x') +
  stat_summary(fun.data = n_fun.hr, geom = "text", vjust = -0.2) +
  theme_USGS_box() +
  theme(strip.text.x.top = element_text(face = "bold"),
        legend.position = "none",
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  scale_fill_manual(values = cbp5) +
  scale_colour_manual(values = cbp5) +
  xlab("") +
  ylab("") +
  stat_pvalue_manual(stat.result, step.increase = 0.01, size = 4, tip.length = 0.01)+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,0.2,0.4,0.6,0.8,1),
                     expand = expansion(mult = c(0.07,0.11)))
pplot.hatchrate



