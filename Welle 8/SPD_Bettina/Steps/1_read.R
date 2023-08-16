# Daten Welle 1-8 einlesen (Longformat)
dat <- 
  read_dta("Input/Erwerbspersonenbefragung_Wellen1-8_long_full_v1-1.dta") %>%
  filter(Stichprobe_w5 == "Basisstichprobe") 


  