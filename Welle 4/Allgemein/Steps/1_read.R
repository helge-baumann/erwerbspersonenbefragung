dat <- read_sav(
  "./Input/Boeckler_Corona_Welle4_2021_final.sav")

names_labs <- map(dat, function(x) attributes(x)$label)
write.csv2(enframe(unlist(names_labs)), "./Output/names_labs_w4.csv")

dat4 <- dat
names(dat4) <- paste0(names(dat4), "__4")

dat_13 <- read_sav(
 "./Input/Erwerbspersonenbefragung_Wellen1-3_wide.sav" 
)

write_dta(dat, "./Input/2020-02-01-064_Kantar_HBS_Erwerbstätige_W4_Vorabdaten.dta")

dat_ges <- dat_13 %>% left_join(dat4, by = c("lfdn__1" = "lfdn_W1__4"))

write_sav(dat_ges, "./Input/Erwerbspersonenbefragung_Wellen1-4_wide.sav")
write_dta(dat_ges, "./Input/Erwerbspersonenbefragung_Wellen1-4_wide.dta")