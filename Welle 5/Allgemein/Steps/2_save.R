# SPSS der aktuellen Welle in Stata konvertieren
write_dta(dat, "./Daten_Export/Boeckler_Corona_Welle5_2021_Kunde_2.dta")

# Wide-Datensatz inklsuive Nachzügler speichern
# zunächst Longformat zurück in Wideformat

dat_full <- dat_long_full %>%
  pivot_wider(id_cols=ID, names_from=Welle, values_from=-c(Welle, ID))

ok <- unname(which(sapply(dat_full, function(x) sum(is.na(x)) < nrow(dat_full))))

dat_full <- dat_full %>% select(ok)
names(dat_full) <- str_remove_all(names(dat_full), "w[:digit:]{1,5}")
  
write_sav(dat_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_wide_full_v2-0.sav")
write_dta(dat_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_wide_full_v2-0.dta")

# reduzierter Wide-Datensatz
dat_wide <- dat_full %>%
  filter(ziehung__5 == 0)

write_sav(dat_wide, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_wide_v2-0.sav")
write_dta(dat_wide, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_wide_v2-0.dta")

# Long

# reduziertes Longformat ohne Nachziehung
dat_long <- dat_long_full %>%
  group_by(ID) %>%
  arrange(ID, Welle) %>% 
  filter(ziehung_w5[5] == 0)

write_dta(dat_long, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_long_v2-0.dta")
write_sav(dat_long, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_long_v2-0.sav")

# Long inklusive Nachzügler
write_dta(dat_long_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_long_full_v2-0.dta")
write_sav(dat_long_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_long_full_v2-0.sav")

