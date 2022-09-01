# SPSS der aktuellen Welle in Stata konvertieren
write_dta(dat8, "./Daten_Export/Boeckler_Corona_Welle8_Mai2022_final_v1-1.dta")
write_sav(dat8, "./Daten_Export/Boeckler_Corona_Welle8_Mai2022_final_v1-1.dta")

# Wide-Datensatz inklsuive Nachzügler speichern
# zunächst Longformat zurück in Wideformat

dat_full <- dat_long_full %>%
  pivot_wider(id_cols=ID, names_from=Welle, values_from=-c(Welle, ID))

ok <- unname(which(sapply(dat_full, function(x) sum(is.na(x)) < nrow(dat_full))))

dat_full <- dat_full %>% select(ok)
names(dat_full) <- str_remove_all(names(dat_full), "w[:digit:]{1,8}")
dat_full <- dat_full %>% select(-paste0("Stichprobe__", c(1:4, 6:8)))
  
write_sav(dat_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-8_wide_full_v1-1.sav")
write_dta(dat_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-8_wide_full_v1-1.dta")

# Long
# Long inklusive Nachzügler
write_dta(dat_long_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-8_long_full_v1-1.dta")
write_sav(dat_long_full, 
          "./Daten_Export/Erwerbspersonenbefragung_Wellen1-8_long_full_v1-1.sav")

