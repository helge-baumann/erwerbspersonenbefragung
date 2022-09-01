dat <- read_sav(
  "./Input/Boeckler_Corona_Welle5_2021_Kunde.sav")

names_labs <- map(dat, function(x) attributes(x)$label)
write.csv2(enframe(unlist(names_labs)), "./Output/names_labs_w5.csv")

dat5 <- dat
names(dat5) <- paste0(names(dat5), "__5")

dat_14 <- read_sav(
 "./Input/Erwerbspersonenbefragung_Wellen1-4_wide.sav" 
)

write_dta(dat, "./Daten_Export/Boeckler_Corona_Welle5_2021_Kunde.dta")

dat_ges <- dat_14 %>% left_join(dat5, by = c("lfdn__1" = "lfdn_W1__5"))

write_sav(dat_ges, "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_wide.sav")
write_dta(dat_ges, "./Daten_Export/Erwerbspersonenbefragung_Wellen1-5_wide.dta")