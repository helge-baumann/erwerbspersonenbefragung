# Daten Welle 8 einlesen
dat8 <- read_sav(
  "./Input/Boeckler_Corona_Welle8_Mai2022_final.sav") %>%
  # ID etwas komplizierter wegen Nachziehung in Welle 5.
  mutate(ziehung = if_else(is.na(lfdn_W1), 1, 0)) %>%
  # fake-ID
  mutate(ID = if_else(ziehung == 1, 
                      # um doppelte IDs zu vermeiden
                      as.double(paste0("5000", lfdn_W5)), 
                      lfdn_W1)) %>%
  select(-ziehung)

# Namen und Labels extrahieren
names_labs <- map(dat8, function(x) attributes(x)$label)
write.csv2(enframe(unlist(names_labs)), "./Output/names_labs_w8.csv")

names(dat8) <- paste0(names(dat8), "__8")

# ältere Datensätze
dat1 <- read_sav("./Input/316402781_Boeckler_Corona_Welle_1_2020_Kunde_final.sav")
names(dat1) <- paste0(names(dat1), "__1")
dat2 <- read_sav("./Input/316402781_Boeckler_Corona_Welle_2_2020_Kunde.sav")
names(dat2) <- paste0(names(dat2), "__2")
dat3 <- read_sav("./Input/Boeckler_Corona_Welle3_2020_Kunde.sav")
names(dat3) <- paste0(names(dat3), "__3")
dat4 <- read_sav("./Input/Boeckler_Corona_Welle4_2021_final.sav")
names(dat4) <- paste0(names(dat4), "__4")
dat5 <- read_sav("./Input/Boeckler_Corona_Welle5_2021_Kunde_2.sav")
names(dat5) <- paste0(names(dat5), "__5")
dat6 <- read_sav("./Input/Boeckler_Corona_Welle6_2021_Kunde_1.sav")
names(dat6) <- paste0(names(dat6), "__6")
dat7 <- read_sav("./Input/Boeckler_Corona_Welle7_Jan2022_final.sav")
names(dat7) <- paste0(names(dat7), "__7")

# Daten mergen (Warnmeldung kann ignoriert werden)
dat_full <- 
  dat1 %>% 
  tibble() %>%
  full_join(dat2, by = c("lfdn__1" = "lfdn_W1__2")) %>%
  full_join(dat3, by = c("lfdn__1" = "lfdn_W1__3")) %>%
  full_join(dat4, by = c("lfdn__1" = "lfdn_W1__4")) %>%
  full_join(dat5, by = c("lfdn__1" = "lfdn_W1__5")) %>%
  # flag (Nachziehung W5)
  mutate(ziehung__5 = if_else(is.na(lfdn__1), 1, 0)) %>%
  # fake-ID
  mutate(ID = if_else(ziehung__5 == 1, 
                      # um doppelte IDs zu vermeiden
                      as.double(paste0("5000", lfdn_W5__5)), 
                      lfdn__1)) %>%
  full_join(dat6, by = c("ID" = "lfdn_W1__6")) %>%
  # hier vorab ID generieren
  full_join(dat7 %>%
              mutate(ziehung__5 = if_else(is.na(lfdn_W1__7), 1, 0)) %>%
              # fake-ID
              mutate(ID = if_else(ziehung__5 == 1, 
                                  # um doppelte IDs zu vermeiden
                                  as.double(paste0("5000", lfdn_W5__7)), 
                                  lfdn_W1__7)) %>%
              select(-ziehung__5),  
            by = "ID") %>%
  # hier vorab ID generieren
  full_join(dat8 %>%
              mutate(ziehung__5 = if_else(is.na(lfdn_W1__8), 1, 0)) %>%
              # fake-ID
              mutate(ID = if_else(ziehung__5 == 1, 
                                  # um doppelte IDs zu vermeiden
                                  as.double(paste0("5000", lfdn_W5__8)), 
                                  lfdn_W1__8)) %>%
              select(-ziehung__5), 
            by = "ID") %>%
  select(ID, everything(), -lfdn__1)
  
# Umbenennung S9_1 für Wellen 3 und 5 (Berufliche Bildung, Namen vorgeschoben)
names(dat_full)[names(dat_full) == "S9_1__3"] <- "S9_1b__3"
names(dat_full)[names(dat_full) == "S9_1__5"] <- "S9_1b__5"

# Faktor Welle 5 & 7 & 8
dat_full <- 
  dat_full %>%
  mutate(
    Faktor_voll__5 = Faktor__5, 
    Faktor_voll__7 = Faktor__7, 
    Faktor_voll__8 = Faktor__8,
    Faktor__5 = Faktor2__5, 
    Faktor__7 = Faktor2__7,
    Faktor__8 = Faktor2__8) %>%
  select(-Faktor2__5, -Faktor2__7, -Faktor2__8)





