dat <- read_sav(
  "./Input/Boeckler_Corona_Welle5_2021_Kunde_2.sav")

names_labs <- map(dat, function(x) attributes(x)$label)
write.csv2(enframe(unlist(names_labs)), "./Output/names_labs_w5.csv")

dat5 <- dat
names(dat5) <- paste0(names(dat5), "__5")

dat_14 <- read_sav(
 "./Input/Erwerbspersonenbefragung_Wellen1-4_wide.sav" 
)

dat_ges <- dat_14 %>% left_join(dat5, by = c("lfdn__1" = "lfdn_W1__5"))

# vollständig inkl Nachzügler
dat_full <- dat_14 %>% 
  full_join(dat5, by = c("lfdn__1" = "lfdn_W1__5")) %>%
  # flag
  mutate(ziehung__5 = if_else(is.na(lfdn__1), 1, 0)) %>%
  # fake-ID
  mutate(ID = if_else(ziehung__5 == 1, 
                      # um doppelte IDs zu vermeiden
                      as.double(paste0("5000", lfdn_W5__5)), 
                      lfdn__1))

# Umbenennung S9_1 für Wellen 3 und 5 (Berufliche Bildung, Namen vorgeschoben)
names(dat_full)[names(dat_full) == "S9_1__3"] <- "S9_1b__3"
names(dat_full)[names(dat_full) == "S9_1__5"] <- "S9_1b__5"


