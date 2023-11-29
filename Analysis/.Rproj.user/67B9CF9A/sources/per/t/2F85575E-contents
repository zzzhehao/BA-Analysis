library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(rstatix)
library(ggh4x)
library(ggsignif)
library(ggpubr)

datapath <- "/Users/hu_zhehao/Library/Mobile Documents/com~apple~CloudDocs/UHH/B.Sc. Biologie/Bachelorarbeit/DNA Samples/Zhehao_Hu_Bachelorthesis_Data.xlsx"
mamas <- read_xlsx(datapath, sheet = "Tetracycline", range = "A3:V61")
kids <- read_xlsx(datapath, sheet = "Tetracycline Offspring", range = "A3:Y51")

# Data preparation
mamas <- mamas %>% filter(!is.na(Gl_ID)) %>% mutate(Partner_M = NA) %>% select(-c(6:13,17,19)) %>% filter(`Initial  Target Copies` != "Positive")
colnames(mamas)[2:3] <- c("PID","OID")
mamas$OID <- as.character(mamas$OID)
mamas$Gl_ID <- as.character(mamas$Gl_ID)

kids <- kids %>% filter(!is.na(Gl_ID)) %>% select(-c(6:14,20,22)) %>% filter(`Initial  Target Copies` != "Positive")
kids$`Initial  Target Copies` <- as.numeric(kids$`Initial  Target Copies`)
mamas$`Initial  Target Copies` <- as.numeric(mamas$`Initial  Target Copies`)
colnames(kids)[2:3] <- c("PID","OID")

df <- bind_rows(mamas, kids) %>% arrange(PID) %>% mutate(Name = case_when(
  is.na(OID) ~ paste0(PID, " Parent"),
  .default = paste0(PID, " ", OID)), .before = PID)
colnames(df)[12] <- "copies"

df <- df %>% filter(PID %in% select(filter(df,duplicated(df$PID)), PID)$PID)
df$copies <- as.numeric(df$copies)

df <- df %>% group_by(PID) %>% mutate(per = copies/max(copies))
df <- df %>% mutate(disp = paste0(round(per*100, digit = 3),"%"))

#### pfin####

df <- df %>% mutate(generation = case_when(
  is.na(OID) ~ "Parent",
  !is.na(OID) ~ "F1"
))
# test
library(rstatix)
library(tidyverse)
library(broom)

f1in.test <- df %>% filter(!is.na(copies) & !is.na(Haplotype))

shapiro <- f1in.test %>% 
  filter(!is.na(Haplotype)) %>%
  group_by(generation, Group) %>%
  nest() %>%
  mutate(Shapiro = map(data, ~shapiro.test(.x$copies)))
shapiro_g <- shapiro %>% 
  mutate(shapiro_g = Shapiro %>% map(glance)) %>%
  unnest(shapiro_g)
shapiro_g

stat.result <- f1in.test %>% ungroup() %>% group_by(Group) %>% t_test(copies ~ generation) %>% add_significance()
stat.result

stat.result <- stat.result %>% add_xy_position(x = "generation", dodge = 0.8)
stat.result <- mutate(stat.result, y.position = (log(y.position, base = 10)+0.5) - 0.05)

#plot
n_fun.fin <- function(x){
  return(data.frame(y = 9,
                    label = length(x)))
}
pfin.ord <- c("Parent", "F1")
pf1in <- 
  df %>% filter(!is.na(Group)) %>% filter(!is.na(Haplotype)) %>%
  ggplot(aes(x = factor(generation, level = pfin.ord), y = copies)) +
  geom_boxplot(aes(fill = Group)) +
  facet_grid(.~Group,scales='free_x') +
  stat_summary(fun.data = n_fun.fin, geom = "text", vjust = -0.2) +
  theme_USGS_box() +
  theme(strip.text.x.top = element_text(face = "bold", size = 10),
        aspect.ratio = 3,
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.line = element_blank()) +
  scale_fill_manual(values = cbp5) +
  xlab("") +
  ylab("") +
  stat_pvalue_manual(stat.result, step.increase = 0.05, size = 3) +
  scale_y_continuous(trans = scales::log10_trans(), 
                     breaks = c(1e3,1e5,1e7, 1e9),
                     expand = expansion(mult = c(0.07,0.12)))
pf1in


# combine with hatchrate (Reproduction.R)

ggsave(ggarrange(pplot.hatchrate, pf1in, labels = c("A","B"), align = "h"), filename = "Plots/tetracycline rep1.png", height = 4, width = 8)


#### Pf Family overview old ####
df <- df %>% mutate(PID = case_when(
  PID == '23Bue16' ~ "Family 1",
  PID == '23Bue161' ~ "Family 2",
  PID == '23Bue166' ~ "Family 3",
  PID == '23Bue176' ~ "Family 4",
  PID == '23Bue179' ~ "Family 5",
  PID == '23Bue125' ~ "Family 6",
  PID == '23Bue173' ~ "Family 7",
  PID == '23Bue239' ~ "Family 8",
  PID == '23Bue25' ~ "Family 9",
  PID == '23Bue333' ~ "Family 10",
  PID == '23Bue349' ~ "Family 11"
))
pf <-
  df %>% arrange(Group) %>%
  ggplot(aes(x=Name, y=copies, fill = Group))+
  geom_bar(stat = "identity") +
  geom_text(aes(label = disp), size = 2, vjust = 3) +
  scale_y_log10(limits = c(1e0, 1e9)) +
  theme_USGS_box() +
  theme.poster() +
  theme(strip.text.x.top = element_text(face = "bold", size = 10),
        aspect.ratio = 0.8,
        legend.position = "none",
        legend.direction = "horizontal",
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(color = "#D0D0D0", fill = NA)) +
  facet_manual(.~Group~PID, design = 
                 "
  AAAABBBCCCDDEE
  IIHHHGGGKKFFJJ
  ",
               scales = "free_x", labeller=function(x) {x[2]}) + #function(x) {x[2]}
  scale_fill_manual(values = cbp5) +
  xlab("") +
  ylab("") +
  labs(title = "Infection level of the offsprings and their mothers")
pf
ggsave("Plots/Offspring Infection Level.png", pf, width = 14, height = 8)

df <- df %>% mutate(parent = case_when(
  is.na(OID) ~ "Parents",
  .default = "F1 Offsprings"
))

#### Pbar family overview new ####
pbar <-
  df %>% filter(!is.na(Group)) %>% filter(!is.na(Haplotype)) %>% arrange(OID) %>%
  ggplot(aes(x=Name, y=copies, fill=PID))+
  geom_bar(stat = "identity") +
  geom_text(aes(label = disp), size = 2, vjust = 3) +
  geom_text(aes(label = Haplotype), size = 2, vjust = 4.5) +
  scale_y_log10(limits = c(1e0,1e9)) +
  theme_USGS_box() +
  theme(strip.text.x.top = element_text(face = "bold", size = 10),
        aspect.ratio = 0.2,
        legend.position = "none",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45),
        axis.line = element_blank()) +
  facet_wrap(.~factor(generation, level = c("Parent", "F1"))~Group, nrow=2, scales = "free_x", labeller = function(x) {x[2]}) +
  xlab("") +
  ylab("") 
pbar

ggsave("Plots/Offspring Infection Level groups.png", pbar, width = 12, height = 5)
