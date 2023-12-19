library(ggplot2)
library(patchwork)
library(dplyr)
library(ggpubr)
#### Danger Zone ####
#0714
w12 <- mcplots[[27]]
w1b <- mcplots[[28]]
w12b <- mcplots[[29]]
#0522
w2b <- mcplots[[12]]
poswa1 <- mcplots[[1]]
neg <- mcplots[[2]]
#0504
dd <- mcplots[[27]]
#0823
poswa2 <- mcplots[[19]]

#### Melt Curve Examples ####
remove_y <- function(){
  theme(
  axis.text.y = element_blank(),
  axis.title.y = element_blank()
  )
}

theme.example <- function() {
  labs(title = "", caption = "")
}

exmp1 <- list(
w2b + xlab("") + theme.example(),
w12 + remove_y() + xlab("") + theme.example(),
w1b + remove_y() + xlab("") + theme.example(),
w12b + remove_y() + xlab("") + theme.example()
)

glob_lab <- "Temperature (ºC)"

p_lab <- 
  ggplot() + 
  annotate(geom = "text", x = 1, y = 1, label = glob_lab, family = "serif", hjust = 5, vjust = 14) +
  coord_cartesian(clip = "off")+
  theme_void()

exmpo1 <- (wrap_plots(exmp1, nrow = 1) | p_lab) + plot_annotation(tag_levels = 'A')
ggsave(exmpo1, filename = "Examples/exmp.png", height = 3, width = 10)
exmpo1


#### Melt Curve scenatio Examples ####
remove_y <- function(){
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )
}

theme.example <- function() {
  labs(title = "", caption = "")
}

exmp2 <- list(
poswa1 + xlab("") + theme.example(),
poswa2 + remove_y() + xlab("") + theme.example(),
neg + remove_y() + xlab("") + theme.example(),
dd + remove_y() + xlab("") + theme.example()
)

glob_lab <- "Temperature (ºC)"

p_lab <- 
  ggplot() + 
  annotate(geom = "text", x = 1, y = 1, label = glob_lab, family = "serif", hjust = 5, vjust = 14) +
  coord_cartesian(clip = "off")+
  theme_void()

exmpo2 <- (wrap_plots(exmp2, nrow = 1) | p_lab) + plot_annotation(tag_levels = 'A')
ggsave(exmpo2, filename = "Examples/exmp2.png", height = 3, width = 10)
exmpo2