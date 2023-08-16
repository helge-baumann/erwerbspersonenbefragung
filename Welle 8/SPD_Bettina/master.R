##############################################################
# Titel: Auswertung Welle 8 (Erwerbspersonenbefragung)
# Autor: Helge Emmler
# R-Version: 4.2.1
# Letztes Update: 19.09.2022
##############################################################


# Präambel: Pakete installieren
  if(!("pacman" %in% installed.packages()[,1])) install.packages("pacman")
  library(pacman)

  # p_load checkt, ob Paket installiert ist; wenn ja: direkt laden
  p_load(haven, dplyr, ggplot2, tidyr, zoo, purrr, tibble, here, openxlsx, 
         stringr, Hmisc, DT, margins)
  
  # here() setzt das Arbeitsverzeichnis an die Stelle des master-files.
  # (Alternative: Projekt anlegen unter File --> New Project)
  setwd(here())

  # Output-Ordner
  dir.create(paste0("Output/", Sys.Date()), showWarnings=F)
  
  
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