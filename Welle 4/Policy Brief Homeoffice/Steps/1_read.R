dat <- read_sav(
  "./Input/Boeckler_Corona_Welle4_2021_final.sav")

dat4 <- dat
names(dat4) <- paste0(names(dat4), "__4")

dat_13 <- read_sav(
 "./Input/Erwerbspersonenbefragung_Wellen1-3_wide.sav" 
)

dat_ges <- dat_13 %>% left_join(dat4, by = c("lfdn__1" = "lfdn_W1__4"))
