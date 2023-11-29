library(readxl)
library(ggpubr)
raw <- read_xlsx("/Users/hu_zhehao/Library/Mobile Documents/com~apple~CloudDocs/UHH/B.Sc. Biologie/Bachelorarbeit/DNA Samples/Zhehao_Hu_Bachelorthesis_Data.xlsx", sheet = "Pool Screen", range = "A4:P113") %>% filter(!is.na(Haplotype)) %>% mutate(Gl_ID = as.character(Gl_ID))

library(dplyr) 
library(stringr)

raw <- raw %>% mutate(Ct = as.numeric(`Ct Mean`)) %>% select(-"Ct Mean")
raw <- bind_rows(raw, rawt)

library(ggplot2)

#### dist grey ####
dist <- raw %>% 
  ggplot()+
  geom_bar(aes(x = Ct, alpha = 0.5), width = 0.10, stat = "count")+
  ylim(c(0,1))+
  xlim(c(15,40))+
  theme_USGS_box()+
  theme(
    aspect.ratio = 0.3,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )+
  xlab("Ct") +
  ylab("")
dist

ggsave("Plots/Poolseq Ct distribution.png", plot = dist, width = 6, height = 3)

#### dist color ####
rawco <- raw %>%
  mutate(`Suggest Result` = as.factor(case_when(
    is.na(`Suggest Result`) ~ NA,
    `Suggest Result` == "Negative" ~ "Negative",
    .default = "Positive"
  ))) %>% filter(!is.na(`Suggest Result`))
dist.co <- rawco %>%
  ggplot()+
  geom_bar(aes(x = Ct, fill = `Suggest Result`), width = 0.10, stat = "count")+
  ylim(c(0,1))+
  xlim(c(15,40))+
  theme_USGS_box()+
  theme(
    aspect.ratio = 0.3,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("Ct") +
  ylab("") +
  scale_fill_manual(values = c("#999999","#DA5858"))
dist.co

ggsave("Plots/Poolseq Ct distribution color.png", plot = dist.co, width = 6, height = 3)

#### HT2 ####
raw <- read_xlsx("/Users/hu_zhehao/Library/Mobile Documents/com~apple~CloudDocs/UHH/B.Sc. Biologie/Bachelorarbeit/DNA Samples/Zhehao_Hu_Bachelorthesis_Data.xlsx", sheet = "Pool Screen", range = "A4:P109") %>% filter(!is.na(Haplotype)) %>% mutate(Gl_ID = as.character(Gl_ID)) %>% mutate(`Initial Target Copies` = as.numeric(`Initial Target Copies`))

dist.ht2 <-
raw %>% filter(Haplotype == "HT2/2st") %>% filter(!is.na(`Initial Target Copies`)) %>%
  ggplot()+
  geom_dotplot(aes(x = "HT2", y = `Initial Target Copies`, fill = `Suggest Result`), binaxis='y', stackdir='center') +
  theme_USGS_box()+
  theme(
    aspect.ratio = 2
  )+
  scale_fill_manual(values = c("#FF904D","#DA5858","#FBD173"))+
  xlab("") +
  ylab("")
ggsave("Plots/Poolseq HT2 Ct distribution.png", plot = dist.ht2, width = 6, height = 6)

