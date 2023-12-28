library(purrr)
library(tidyverse)

c("readxl", "plyr", "purrr", "rlist", "dplyr", "ggplot2", "tidyr", "stringr", "tidyverse", "ggpubr", "gridExtra", "knitr", "MASS", "rstatix", "reshape2", "scales", "ggh4x", "ggsignif", "broom") %>%
  map(citation) %>%
  print(style = "text")

devtools::session_info()

citation()
version$version.string
RStudio.Version()
