# Aufbereitung Erwerbspersonenbefragung Welle 6
# Helge Emmler
# R-Version: 4.1.1
# Letztes Update: 25.10.2021

if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
library(pacman)
p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, openxlsx, stringr, 
       Hmisc)

# Alle Unterdateien ausführen

# selbstgeschriebene Funktionen
source("./Functions/functions.R", encoding="UTF-8")

# alle Unterdateien im UNterordner "Steps" der Reihe nach ausführen
n <- 1:length(dir("./Steps"))
sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")

# sessionInfo (geladene Pakete etc. für Fehlersuche)
dir.create("./sessionInfo()", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./sessionInfo()/", format(Sys.time(), "%y%m%d"), ".txt")
)
