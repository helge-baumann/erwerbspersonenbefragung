# Verteilung der Homeoffice-Variablen----
ho <- dat_ges %>%
  dplyr_relfreq(x = W4_1__4, gew = Faktor__4) %>%
  mutate(dec = anteil)
ho$jan <- dat_ges %>%
  dplyr_relfreq(x = A2__4, gew = Faktor__4) %>%
  pull(anteil)

# Verteilung von April 2020 bis November
ho$apr <- dat_ges %>%
  dplyr_relfreq(x = F2__1, gew = Faktor__1) %>%
  pull(anteil)
ho$jun <- dat_ges %>%
  dplyr_relfreq(x = A2__2, gew = Faktor__2) %>%
  pull(anteil)
ho$nov <- dat_ges %>%
  dplyr_relfreq(x = A2__3, gew = Faktor__3) %>%
  pull(anteil)
ho$vor <- c(dat_ges %>%
  dplyr_relfreq(x = F3__1, gew = Faktor__1) %>%
  pull(anteil), 0)

ho <- ho %>%
  select(x, vor, apr, jun, nov, dec, jan)
write.csv2(ho, "./Output/Homeoffice_Zeitreihe.csv")


# Verteilung nach Personen differenziert----

# Eltern
dat_ges <- dat_ges %>% mutate(
  eltern1 = case_when(
    S11_2__1 %in% 2:10 & S11_3__1 == 1 ~ "Kinder ab 14 und unter 18",
    S11_3__1 %in% 2:10 ~ "Kinder unter 14",
    S11_2__1 == 1 & S11_3__1 == 1 ~ "keine Kinder",
  )
)
#eltern <- ho_extract(x=eltern, kat=na.omit(unique(dat_ges$eltern)))
#write.csv2(eltern, "./Output/Homeoffice_Zeitreihe_Eltern.csv")

eltern2 <- ho_extract(x=A2j__2, kat=c("Ja", "Nein"))
write.csv2(eltern2, "./Output/Homeoffice_Zeitreihe_Eltern2.csv")

# Geschlecht
sex <- ho_extract(x=S2__1, kat=c("Männlich", "Weiblich"))
write.csv2(sex, "./Output/Homeoffice_Zeitreihe_Geschlecht.csv")

# Alter
dat_ges <- dat_ges %>%
  mutate(alter_g = cut(
    S1__1, breaks=c(15, 35, 55, 100), 
    labels=c("bis 35 Jahre", "36 bis 55 Jahre", "56 und älter")
  ))

alter <- ho_extract(x=alter_g, kat=levels(dat_ges$alter_g))
write.csv2(alter, "./Output/Homeoffice_Zeitreihe_Alter.csv")

# Einkommen
dat_ges <- dat_ges %>%
  mutate(eink_g = cut(
    S10__1, breaks=c(0, 4, 7, 9, 11), 
    labels=c("bis unter 1.500 Euro", "1.500 bis unter 2.600 Euro", 
             "2.600 bis unter 4.500 Euro", "4.500 Euro und mehr")
  ))

eink <- ho_extract(x=eink_g, kat=levels(dat_ges$eink_g))
write.csv2(eink, "./Output/Homeoffice_Zeitreihe_Einkommen.csv")

# Betriebe
br <-  ho_extract(x=F34__1, kat=c("Ja", "Nein"))
tarif <-  ho_extract(x=F33__1, kat=c("Ja", "Nein"))
bg <-  ho_extract(x=F32__1, kat=levels(as_factor(dat_ges$F32__1)))
branche <-  ho_extract(x=S5__1, kat=levels(as_factor(dat_ges$S5__1)))

write.csv2(br, "./Output/Homeoffice_Zeitreihe_BR.csv")
write.csv2(tarif, "./Output/Homeoffice_Zeitreihe_Tarifbindung.csv")
write.csv2(bg, "./Output/Homeoffice_Zeitreihe_Betriebsgröße.csv")
write.csv2(branche, "./Output/Homeoffice_Zeitreihe_Branche.csv")


# Bewegungen

dat_ges <- dat_ges %>%
  mutate(
    apr_nov = if_else(F2__1 %in% c(2,3) & A2__3 %in% c(2,3), 1, 0),
    apr_jan = if_else(F2__1 %in% c(2,3) & A2__4 %in% c(2,3), 1, 0)) 
# Insgesamt
dat_ges %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(F2__1, gew=Faktor__4)
dat_ges %>% filter(A2__4 %in% c(1)) %>% dplyr_relfreq(F2__1, gew=Faktor__4)

apr_nov_jan_branche <- dat_ges %>% 
  filter(!is.na(Faktor__3) & !is.na(Faktor__4) & F2__1 %in% c(2,3)) %>%
  dplyr_relfreq2(S5__1, apr_nov, Faktor__3) %>%
  filter(y == 1) %>%
  select(x, anteil, n) 

apr_nov_jan_branche$jan <- dat_ges %>% 
  filter(!is.na(Faktor__3) & !is.na(Faktor__4) & F2__1 %in% c(2,3)) %>%
  dplyr_relfreq2(S5__1, apr_jan, Faktor__4) %>%
  filter(y == 1) %>%
  pull(anteil) 

write.csv2(apr_nov_jan_branche, "./Output/Change_Branche.csv")

apr_nov_jan_gk <- dat_ges %>% 
  filter(!is.na(Faktor__3) & !is.na(Faktor__4) & F2__1 %in% c(2,3)) %>%
  dplyr_relfreq2(F32__1, apr_nov, Faktor__3) %>%
  filter(y == 1) %>%
  select(x, anteil, n) 

apr_nov_jan_gk$jan <- dat_ges %>% 
  filter(!is.na(Faktor__3) & !is.na(Faktor__4) & F2__1 %in% c(2,3)) %>%
  dplyr_relfreq2(F32__1, apr_jan, Faktor__4) %>%
  filter(y == 1) %>%
  pull(anteil) 

write.csv2(apr_nov_jan_gk, "./Output/Change_GK.csv")

  
  
change <- dat %>% 
  filter(!is.na(change)) %>%
  select(change, S5, S2, starts_with("S11"), , A3, S3__3, starts_with("A2b"), 
         F5__3, A5__3, D2__3, D4__3, D7__3, D8__3, D9__3, Faktor) %>%
  mutate_at(vars(-Faktor), as_factor) %>%
  pivot_longer(cols=-c(change, Faktor), names_to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert, change) %>%
  summarise(anteil = sum(Faktor), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n)) %>%
  filter(!(Wert %in% c("NA", "Weiß nicht", "Keine Angabe")))
  
write.csv2(change, "./Output/Change.csv")

zukunft <- dat %>% filter(A2 %in% 2:3) %>% dplyr_relfreq(A2d, gew=Faktor)

# Thesen, Einstellungen
thesen <- data.frame(
x1 = c(
  dat_ges %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_6__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_6__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_6__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
), 
x2 = c(
NA, 
dat_ges %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_10__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_10__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
), 
x3 = c(
dat_ges %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_4__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_4__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_4__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
),
x4= c(
dat_ges %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_2__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_2__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_2__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
),
x5 = c(
dat_ges %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_5__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_5__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
dat_ges %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_5__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
)
)

thesen <- t(thesen)
row.names(thesen) <- c(
  attributes(dat_ges$A2c_6__4)$label,
  attributes(dat_ges$A2c_10__4)$label,
  attributes(dat_ges$A2c_4__4)$label,
  attributes(dat_ges$A2c_2__4)$label,
  attributes(dat_ges$A2c_5__4)$label
  
)

write.csv2(thesen, "./Output/Thesen_Homeoffice.csv")

dat_ges2 <- dat_ges %>% filter(F2__1 %in% c(2,3) & A2__3 %in% c(2,3) & A2__4 %in% c(2,3))
thesen_laengs <- data.frame(
  x1 = c(
    dat_ges2 %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_6__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_6__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_6__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
  ), 
  x2 = c(
    NA, 
    dat_ges2 %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_10__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_10__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
  ), 
  x3 = c(
    dat_ges2 %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_4__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_4__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_4__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
  ),
  x4= c(
    dat_ges2 %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_2__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_2__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_2__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
  ),
  x5 = c(
    dat_ges2 %>% filter(F2__1 %in% c(2,3)) %>% dplyr_relfreq(F21_5__1, gew=Faktor__1) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__3 %in% c(2,3)) %>% dplyr_relfreq(A2c_5__3, gew=Faktor__3) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum(),
    dat_ges2 %>% filter(A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2c_5__4, gew=Faktor__4) %>% filter(row_number() <=2) %>% pull(anteil) %>% sum()
  )
)

# 
  dat_ges %>% filter(A2__2 %in% c(2,3) & A2__3 %in% c(2,3) & A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2d__2, gew=Faktor__2) 
  dat_ges %>% filter(A2__2 %in% c(2,3) & A2__3 %in% c(2,3) & A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2d__3, gew=Faktor__3)
  dat_ges %>% filter(A2__2 %in% c(2,3) & A2__3 %in% c(2,3) & A2__4 %in% c(2,3)) %>% dplyr_relfreq(A2d__4, gew=Faktor__4)
  