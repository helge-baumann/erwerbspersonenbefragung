###########################
# Homeoffice für PM / Aktualisierung PB
# R 4.3.1
# Helge Emmler
# 15.08.2023
###########################

# Präambel
if(!"pacman" %in% installed.packages()[,1]) install.packages("pacman")
library(pacman)

p_load(tidyverse, haven)

# Skripte durchlaufen
map(dir("./steps", full.names=T), source, encoding="UTF-8")

# sessionInfo
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_",format(Sys.time(), "%y%m%d"), ".txt")
)
