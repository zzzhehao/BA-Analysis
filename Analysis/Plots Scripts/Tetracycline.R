library(readxl)
library(dplyr) 
library(stringr)
library(ggplot2)
library(ggpubr)
library(plyr)
library(reshape2)
library(gridExtra)
library(ggsignif)

raw <- read_xlsx("/Users/hu_zhehao/Library/Mobile Documents/com~apple~CloudDocs/UHH/B.Sc. Biologie/Bachelorarbeit/DNA Samples/Zhehao_Hu_Bachelorthesis_Data.xlsx", sheet = "Tetracycline", range = "A3:U61") %>% filter(!is.na(Group)) %>% filter(!is.na(Gl_ID))

raw <- raw %>% select(c(1,4,5,14,15,16,20,21))
colnames(raw)[8] <- "quantity"
raw <- raw %>% 
  mutate(Interpretation = case_when(
    `Suggest result` == "Negative" ~ "negative",
    `quantity` == "Positive" ~ "positive",
  )) %>%
  mutate(Interpretation = replace(Interpretation, is.na(Interpretation), "quantified"))
raw$Interpretation <- as.factor(raw$Interpretation)
r.quantified <- filter(raw, Interpretation == "quantified") %>% filter(!is.na(Haplotype) & !is.na(Group))
r.quantified$`quantity` <- as.numeric(r.quantified$`quantity`) 

#### Plot theme ####
cbp5 <- (c("#888888","#35CCC9")) # Control, etracycline
theme_USGS_box <- function(base_family = "serif", ...){
  theme_bw(base_family = base_family, ...) +
    theme(
      plot.title = element_text(size = 12),
      axis.ticks.length = unit(-0.05, "in"),
      axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.ticks.x = element_blank(),
      aspect.ratio = 1,
      legend.background = element_rect(color = "black", fill = "white"),
      panel.background = element_rect(fill = "grey96", colour = "grey20")
    )
}

#### Test ####
library(rstatix)
library(tidyverse)
library(broom)

tc.test <- r.quantified

shapiro <- tc.test %>% 
  filter(!is.na(Haplotype)) %>%
  group_by(Group, Haplotype) %>%
  nest() %>%
  mutate(Shapiro = map(data, ~shapiro.test(.x$quantity)))
shapiro_g <- shapiro %>% 
  mutate(shapiro_g = Shapiro %>% map(glance)) %>%
  unnest(shapiro_g)
shapiro_g

stat.result <- tc.test %>% ungroup() %>% filter(!is.na(quantity)) %>% group_by(Haplotype) %>% t_test(quantity ~ Group) %>% add_significance()
stat.result

stat.result <- stat.result %>% add_xy_position(x = "Group", dodge = 0.8)
stat.result <- mutate(stat.result, y.position = log(y.position, base = 10))


#### C Quantified: quantity_groups vars Haplotype ####
n_fun.p1 <- function(x){
  return(data.frame(y = 8.3,
                    label = length(x)))
}
n.haplotypes.p1 <- c(
  'HT1' = "HT1",
  'HT1st' = "HT1*",
  'HT2/2st' = "HT2/2*"
)
colnames(r.quantified)[8] <- "quantity"
p.quantity_group.quantified <- 
  r.quantified %>% filter(!is.na(Haplotype)) %>%
  ggplot(aes(x=Group,y=quantity)) + 
  geom_boxplot(aes(fill=Group)) +
  scale_y_log10() +
  facet_grid(.~Haplotype,labeller=as_labeller(n.haplotypes.p1),scales='free_x') +
  stat_summary(fun.data = n_fun.p1, geom = "text", hjust = 0.5) +
  theme_USGS_box() +
  theme(strip.text.x.top = element_text(face = "bold", size = 10),
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  ) +
  scale_fill_manual(values = cbp5) +
  xlab("") +
  ylab("Initial wsp-Gene Copies") +
  stat_pvalue_manual(stat.result, step.increase = 0.01, size = 4, tip.length = 0.01)+
  scale_y_continuous(trans = scales::log10_trans(), 
                     breaks = c(1e5,1e7,1e8),
                     expand = expansion(mult = c(0.07,0.10)))
p.quantity_group.quantified

ggsave(ggarrange(p.bar.infection_categories,p.quantity_group.quantified, labels = c("A","B"), nrow = 1, widths = c(3,3)), 
       filename = "Plots/tetracycline1.png", width = 10, height = 5)

#### A Bar: categories of infection status ####
n_fun.pA <- function(x){
  return(data.frame(y = -18,
                    label = length(x)))
}
n.haplotypes.pA <- c(
  'HT1' = "HT1",
  'HT1st' = "HT1*",
  'HT2/2st' = "HT2/2*"
)
p.bar.infection_categories <- 
  raw %>% filter(!is.na(Haplotype)) %>%
  ggplot(aes(x = Interpretation, fill = Group))+
  geom_histogram(stat = "count", binwidth = 1500) +
  stat_count(binwidth = 1500, 
             geom = "text", 
             color = "white", 
             aes(label = after_stat(count),
                 group = Group),
             size = 3.5,
             position = position_stack(vjust = 0.55)) +
  facet_wrap(vars(Haplotype),labeller=as_labeller(n.haplotypes.pA),scales='free_x') +
  theme_USGS_box() +
  theme(strip.text.x.top = element_text(face = "bold", size = 10),
        legend.position = "none",
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  ) +
  scale_fill_manual(values = cbp5) +
  scale_color_manual(values = c(NA, NA, "red"))+
  xlab("") +
  ylab("Number of Observations")
p.bar.infection_categories

#### B All Data: Ct_groups vars Haplotype ####
n_fun.p2 <- function(x){
  return(data.frame(y = -18,
                    label = length(x)))
}
n.haplotypes.p2 <- c(
  'HT1' = "HT1\n(N=14)",
  'HT1st' = "HT1*\n(N=3)",
  'HT2/2st' = "HT2/2*\n(N=20)"
)
p.ct_group.all <- raw %>% filter(`Suggest result`!="Negative") %>%
  ggplot(aes(x=Group,y=Ct,fill=Group)) +
  geom_boxplot() + 
  scale_y_reverse() +
  facet_grid(.~Haplotype,labeller=as_labeller(n.haplotypes.p2),scales='free_x') +
  stat_summary(fun.data = n_fun.p2, geom = "text", hjust = 0.5) +
  theme_USGS_box() +
  theme(strip.text.x.top = element_text(face = "bold"),
        legend.direction = "none",
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = cbp1) +
  scale_colour_manual(values = cbp1) +
  xlab("") +
  ylab("Ct") +
  labs(title = "All Samples")


#### Tests ####
library(rstatix)
raw$quantity <- as.numeric(raw$quantity)
rawtest <- raw %>% filter(!is.na(Haplotype) & !is.na(Group)) %>% ungroup() %>% filter(!is.na(quantity)) %>% mutate(testg = as.factor(paste0(Haplotype, Group))) %>% t_test(quantity ~ testg)
rawtest


#### Poster ####
# C
pC <- 
  ggplot(raw, aes(x = Interpretation, fill = Group, color = Group))+
  geom_histogram(stat = "count", binwidth = 1500) +
  stat_count(binwidth = 1500, 
             geom = "text", 
             color = "white", 
             aes(label = after_stat(count),
                 group = Group),
             size = 3,
             position = position_stack(vjust = 0.55)) +
  facet_wrap(vars(Haplotype),labeller=as_labeller(n.haplotypes.pA),scales='free_x') +
  theme_USGS_box() +
  theme.poster() +
  theme(strip.text.x.top = element_text(face = "bold"),
        legend.position = c(0.5,-0.3),
        legend.direction = "horizontal",
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(color = "#FFFFFF"),
        axis.line = element_blank(),
        panel.border = element_rect(color = "#D0D0D0", fill = NA)
  ) +
  scale_fill_manual(values = cbp5) +
  scale_colour_manual(values = c("#D0D0D0","#D0D0D0")) +
  labs(title = "Infection status of adult beetles") +
  xlab("") +
  ylab("")
ggsave("Plots/test.png")
pC

# D
pD <- ggplot(r.quantified,aes(x=Group,y=quantity,fill=Group,color=Group)) + 
  geom_boxplot(width = 0.5) +
  scale_y_log10(limits = c(1e6,1e8)) +
  facet_grid(.~Haplotype,labeller=as_labeller(n.haplotypes.p1),scales='free_x') +
  stat_summary(fun.data = n_fun.p1, geom = "text", vjust = 1, color = "white") +
  theme_USGS_box() +
  theme.poster() +
  theme(strip.text.x.top = element_text(face = "bold"),
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "#D0D0D0", fill = NA)
  ) +
  scale_fill_manual(values = cbp5) +
  scale_colour_manual(values = c("#D0D0D0","#D0D0D0")) +
  xlab("") +
  ylab("") +
  labs(title = "Quantified Samples")
ggsave("Plots/test.png")
pD

n.haplotypes.p1