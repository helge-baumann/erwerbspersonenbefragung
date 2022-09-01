##############################################################
# Titel: Homeoffice (Herbstforum)
# Autor: Helge Emmler
# R-Version: 4.1.1
# Letztes Update: 04.11.2021
##############################################################


# Präambel: Pakete installieren
  if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
  library(pacman)

  # p_load checkt, ob Paket installiert ist; wenn ja: direkt laden
  p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, here, Hmisc, stringr)
  
  # here() setzt das Arbeitsverzeichnis an die Stelle des master-files.
  # (Alternative: Projekt anlegen unter File --> New Project)
  setwd(here())

  
# Alle Unterdateien ausf?hren

  # selbstgeschriebene Funktionen
  source("./Functions/functions.R", encoding="UTF-8")
  
  # alle Unterdateien im UNterordner "Steps" der Reihe nach ausf?hren
  n <- 1:length(dir("./Steps"))
  sapply(dir("./Steps", full.names=T)[n], source, encoding="UTF-8")
  
for(b in dir("Output")) file.copy(from=paste0("Output/", b), to=paste0("../Skript/Input/", b))

  
# sessionInfo (geladene Pakete etc. f?r Fehlersuche)
  dir.create("./sessionInfo()", showWarnings=F)
  writeLines(
    capture.output(sessionInfo()), 
    con=paste0("./sessionInfo()/", format(Sys.time(), "%y%m%d"), ".txt")
  )