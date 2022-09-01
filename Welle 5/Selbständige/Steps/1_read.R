# Daten mit read_dta() oder read_sav() einlesen, 
# mit write_sav oder write_dta() aufs Laufwerk schreiben. 

# Daten sollten im Unterordner /Input liegen.
dat <- read_sav("./Input/Erwerbspersonenbefragung_Wellen1-5_long_v1-2.sav")
dat5 <- read_sav("./Input/Boeckler_Corona_Welle5_2021_Kunde_2.sav")
