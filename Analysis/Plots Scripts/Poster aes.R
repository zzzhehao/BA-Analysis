theme.poster <- function(base_family = "serif", ...){
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.background = element_rect(fill = "transparent",
                                      colour = NA_character_),
      plot.background = element_rect(fill = "transparent",
                                     colour = NA_character_),
      legend.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
      legend.box.background = element_rect(fill = "transparent",
                                           colour = NA_character_),
      legend.key = element_rect(fill = "transparent",
                                colour = NA_character_),
      plot.title = element_text(color = "#FFFFFF"),
      axis.line = element_line(color = "#FFFFFF"),
      legend.text = element_text(colour = "#FFFFFF"),
      legend.title = element_text(colour = "#FFFFFF"),
      axis.text = element_text(colour = "#D0D0D0")
    )
}

P1 <- ggarrange(pA,
                labels = c("A"),
                font.label = list(color = "#FFFFFF"))
ggsave("Plots/P1.png", P1, width = 6, height = 2.5, bg = "#303030")

P2 <- ggarrange(pB,
                labels = c("B"),
                font.label = list(color = "#FFFFFF"))
ggsave("Plots/P2.png", P2, width = 4, height = 3.5, bg = "#303030")

P3 <- ggarrange(pC, pD, pE,
                labels = c("C", "D", "E"), 
                nrow = 1,
                font.label = list(color = "#FFFFFF"))
ggsave("Plots/P3.png", P3, width = 9 , height = 3.5, bg = "#303030")

P4 <- ggarrange(pF,
                labels = c("F"), 
                nrow = 1,
                font.label = list(color = "#FFFFFF"))
ggsave("Plots/P4.png", P4, width = 10 , height = 5, bg = "#303030")


PP1 <- ggarrange(pA, pB, pC,
              labels = c("A", "B", "C"),
              nrow = 1,
              widths = c(5.5,3.3,2.4),
              font.label = list(color = "#FFFFFF"))
ggsave("Plots/PP1.png", PP1, 
       width = 15, height = 3, bg = "#303030")


PP2 <- ggarrange(pD, pE, pF,
                 labels = c("A", "B", "C"),
                 nrow = 1,
                 widths = c(3.7,4.3,8),
                 font.label = list(color = "#FFFFFF"))
ggsave("Plots/PP2.png", PP2, 
       width = 15, height = 5, bg = "#303030")
