# Auswertung Erwerbspersonenbefragung Welle 4
# Helge Emmler
# Letztes Update: 02.02.2021

if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, openxlsx, stringr)

source("./Functions/functions.R", encoding="UTF-8")
n <- 1:length(dir("./Steps"))
sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")