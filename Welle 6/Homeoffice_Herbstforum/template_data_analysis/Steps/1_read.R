# Daten mit read_dta() oder read_sav() einlesen, 
# mit write_sav oder write_dta() aufs Laufwerk schreiben. 

# Daten sollten im Unterordner /Input liegen.
dat_roh <- read_dta("Input/Erwerbspersonenbefragung_Wellen1-6_long_full_v1-0.dta")
