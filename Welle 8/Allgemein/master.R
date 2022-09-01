# Aufbereitung Erwerbspersonenbefragung Welle 8
# Helge Emmler
# R-Version: 4.2.1
# Letztes Update: 16.08.2022

if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, openxlsx, stringr, 
       Hmisc)

# Alle Unterdateien ausführen

# selbstgeschriebene Funktionen
source("./Functions/functions.R", encoding="UTF-8")

# alle Unterdateien im Unterordner "Steps" der Reihe nach ausführen
n <- 1:length(dir("./Steps")[str_detect(dir("./Steps"), ".R$")])
sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")

# sessionInfo (geladene Pakete etc. für Fehlersuche)
dir.create("./sessionInfo()", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./sessionInfo()/", format(Sys.time(), "%y%m%d"), ".txt")
)
