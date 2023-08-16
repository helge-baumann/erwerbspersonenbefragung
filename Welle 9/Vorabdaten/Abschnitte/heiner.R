# Feldzeit
dat %>% 
  filter(Welle == 9) %>% 
  ggplot(aes(x=intervtag)) +
  geom_bar(fill="blue")


# Streik komplett (alt)----
 dat %>% 
  filter(Welle == 9) %>% 
  mutate(`(1) Gesamt` = "Gesamt", Branche = as_factor(w3_branche),
         Erwerbstätigkeit = if_else(erwt %in% 1:2, "Erwerbstätig", "Nicht erwerbstätig"), Sonntagsfrage = as_factor(sonntagsfrage),
         Tätigkeit = case_when(taetigk %in% 1:3 ~ "Arbeiter:innen, Angestellte, Beamte", taetigk %in% 4:7 ~ "Selbststaendige und Mithelfende Familienangehörige"))  %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Erwerbstätigkeit, Tätigkeit, Branche, Sonntagsfrage)) %>%
  mutate(`(1) Gesamt` = "Gesamt", Gewerkschaftsmitglied = case_when(gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")) %>% 
  
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied, Region, Geschlecht), names_to="name2", values_to="value2") %>%
  mutate(`(1) Gesamt` = "Gesamt", Gewerkschaftsmitglied = case_when(gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")) %>% 
  
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied), names_to="name3", values_to="value3") %>%
 
  filter(value > 0 & value2 > 0 & streik >= 0 ) %>% 
  group_by(name, value, name2, value2, name3, value3, streik) %>%
  filter(!is.na(gewicht_alle)) %>% 
  summarise(N=n(), p= sum(gewicht_alle, na.rm=T)) %>%
  mutate(W = round(p/sum(p)*100), N=sum(N)) %>%
  
  pivot_wider(id_cols=c(name, value, value2, value3), 
              names_from=streik, values_from=c(N, W)) %>% 
  write.csv2("output/streik.csv", fileEncoding="CP1252")

dat %>% 
  filter(Welle == 9) %>% 
  mutate(`(1) Gesamt` = "Gesamt", Gewerkschaftsmitglied = case_when(gewerk == 1 ~ "Gewerkschaftsmitglied", 
                                                                    gewerk == 2 ~ "kein Gewerkschaftsmitglied")) %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied, Region), names_to="name", values_to="value") %>%
  filter(value > 0 
         & streik_ztp >= 0) %>% 
  group_by(name, value, streik_ztp) %>%
  filter(!is.na(gewicht_alle)) %>% 
  summarise(N=n(), p= sum(gewicht_alle, na.rm=T)) %>%
  mutate(W = round(p/sum(p)*100), N=sum(N), streik_ztp = as_factor(streik_ztp)) %>%
  
  pivot_wider(id_cols=c(name, value, N), 
              names_from=streik_ztp, values_from=c(W)) %>% 
  write.csv2("output/streik_ztp.csv", fileEncoding="CP1252")

# Streik Januar 2023
dat %>% 
  filter(Welle == 9) %>% 
  # Ebene I
  mutate(`(1) Gesamt` = "Gesamt", 
         Tätigkeit = case_when(
           taetigk %in% 1 ~ "Arbeiter:innen", 
           taetigk %in% 2 ~ "Angestellte",
         taetigk %in% 3 ~ "Beamte"),
         `Alter gruppiert` = as_factor(case_when(
           alter >= 0 & alter <= 25 ~ "1. 21 Bis einschließlich 25", 
           alter >= 26 & alter <= 45 ~ "2. 26 einschließlich 45", 
           alter >= 46  ~ "3. 46 und älter"
         )
         ))  %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Tätigkeit, Äquivalenzeinkommen, `Alter gruppiert`)) %>%
  # Ebene II
  mutate(`(1) Gesamt` = "Gesamt", Gewerkschaftsmitglied = case_when(
    gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")
    ) %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied), 
               names_to="name2", values_to="value2") %>%
  filter(value > 0 & value2 > 0 & streik >= 0 ) %>% 
  group_by(name, value, name2, value2, streik) %>%
  filter(!is.na(gewicht_alle)) %>% 
  summarise(N=n(), p= sum(gewicht_alle, na.rm=T)) %>%
  mutate(W = round(p/sum(p)*100), N=sum(N)) %>%
  pivot_wider(id_cols=c(name, value, value2), 
              names_from=streik, values_from=c(N, W)) %>% 
  write.csv2("output/streik_2023-01-09.csv", fileEncoding="CP1252")

# Zeitpunkte
dat %>% 
  filter(Welle == 9) %>% 
  mutate(`(1) Gesamt` = "Gesamt", 
         Tätigkeit = case_when(
           taetigk %in% 1:2 ~ "Arbeiter:innen, Angestellte (ohne Beamte)"),
         Gewerkschaftsmitglied = case_when(
           gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")
         )  %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied, Tätigkeit, Region)) %>%
  # Ebene II
  mutate(`(1) Gesamt` = "Gesamt", Gewerkschaftsmitglied = case_when(
    gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")
  ) %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied), 
               names_to="name2", values_to="value2") %>%
  filter(value > 0 
         & streik_ztp >= 0) %>% 
  group_by(name, value, name2, value2, streik_ztp) %>%
  filter(!is.na(gewicht_alle)) %>% 
  summarise(N=n(), p= sum(gewicht_alle, na.rm=T)) %>%
  mutate(W = round(p/sum(p)*100), N=sum(N), streik_ztp = as_factor(streik_ztp)) %>%
  
  pivot_wider(id_cols=c(name, value, value2, N), 
              names_from=streik_ztp, values_from=c(W)) %>% 
  write.csv2("output/streik_ztp_2023-01-05.csv", fileEncoding="CP1252")

# Heiner Mail 16.01.2023

# Anteil Gewerkschaftsmitglieder unter allen mit Streikerfahrung
dat %>% 
  filter(streik %in% 1:2) %>% 
  group_by(gewerk) %>% 
  summarise(n = sum(gewicht)) %>% 
  mutate(p=n/sum(n))

# Februar 2023----
dat %>% 
  filter(Welle == 9 & taetigk %in% 1:3) %>% 
  # Ebene I
  mutate(`(1) Gesamt` = "Gesamt",
         `Alter gruppiert` = 
           as_factor(
             case_when(
               alter >= 0 & alter <= 25 ~ "1. 21 Bis einschließlich 25", 
               alter >= 26 & alter <= 45 ~ "2. 26 einschließlich 45", 
               alter >= 46  ~ "3. 46 und älter"
               )
             ),
         Tarifbindung = as_factor(case_when(tarif == 1 ~ "Ja", tarif == 2 ~ "Nein")),
         Betriebsrat = 
           as_factor(
             case_when(
               w8_br == 1 ~ "Betriebsrat", w8_br == 2 ~ "Personalrat", w8_br == 3 ~ "Weder noch"
             )
           ),
         Gewerkschaftsmitglied = case_when(
           gewerk == 1 ~ "Gewerkschaftsmitglied", 
           gewerk == 2 ~ "kein Gewerkschaftsmitglied"
           )
         )  %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Tarifbindung, Betriebsrat)) %>%
  # Ebene II
  mutate(`(1) Gesamt` = "Gesamt") %>% 
  pivot_longer(
    cols=c(`(1) Gesamt`, Gewerkschaftsmitglied, Äquivalenzeinkommen, `Alter gruppiert`, Region), 
               names_to="name2", values_to="value2") %>%
  mutate(
    Streik = 
      case_when(
        streik == 1 ~ "Ja, einmal", streik == 2 ~ "Ja, mehrmals", streik == 3 ~ "Nein, nie"
        ),
    Streik_12mon = case_when(streik_ztp == 1 ~ "Ja", streik_ztp %in% 2:4 | streik == 3 ~ "Nein"),
    Vertrauen_Gew = case_when(instv_mp_gew %in% 4:5 ~ "Großes oder sehr großes Vertrauen",
                              instv_mp_gew == 3 ~ "Mittleres Vertrauen",
                              instv_mp_gew %in% 1:2 ~ "Wenig oder überhaupt kein Vertrauen"),
    Vertrauen_Partei = 
    ) %>% 
  pivot_longer(
    cols=c(Streik, Streik_12mon, Vertrauen_Gew), 
    names_to="name3", values_to="value3") %>%
  filter(value > 0 & value2 > 0 & value3 >= 0 ) %>% 
  group_by(name, value, name2, value2, name3, value3) %>%
  filter(!is.na(gewicht)) %>% 
  summarise(N=n(), p= sum(gewicht, na.rm=T)) %>%
  mutate(W = round(p/sum(p)*100), N=sum(N)) %>%
  pivot_wider(id_cols=c(name, value, value2), 
              names_from=value3, values_from=c(N, W)) %>% 
  write.csv2("output/streik_2023-02-16.csv", fileEncoding="CP1252")

# Zeitpunkte
dat %>% 
  filter(Welle == 9) %>% 
  mutate(`(1) Gesamt` = "Gesamt", 
         Tätigkeit = case_when(
           taetigk %in% 1:2 ~ "Arbeiter:innen, Angestellte (ohne Beamte)"),
         Gewerkschaftsmitglied = case_when(
           gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")
  )  %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied, Tätigkeit, Region)) %>%
  # Ebene II
  mutate(`(1) Gesamt` = "Gesamt", Gewerkschaftsmitglied = case_when(
    gewerk == 1 ~ "Gewerkschaftsmitglied", gewerk == 2 ~ "kein Gewerkschaftsmitglied")
  ) %>% 
  pivot_longer(cols=c(`(1) Gesamt`, Gewerkschaftsmitglied), 
               names_to="name2", values_to="value2") %>%
  filter(value > 0 
         & streik_ztp >= 0) %>% 
  group_by(name, value, name2, value2, streik_ztp) %>%
  filter(!is.na(gewicht_alle)) %>% 
  summarise(N=n(), p= sum(gewicht_alle, na.rm=T)) %>%
  mutate(W = round(p/sum(p)*100), N=sum(N), streik_ztp = as_factor(streik_ztp)) %>%
  
  pivot_wider(id_cols=c(name, value, value2, N), 
              names_from=streik_ztp, values_from=c(W)) %>% 
  write.csv2("output/streik_ztp_2023-01-05.csv", fileEncoding="CP1252")

# Heiner Mail 16.01.2023

# Anteil Gewerkschaftsmitglieder unter allen mit Streikerfahrung
dat %>% 
  filter(streik %in% 1:2 & taetigk %in% 1:3) %>% 
  group_by(gewerk) %>% 
  summarise(n = sum(gewicht)) %>% 
  mutate(p=n/sum(n))

# Anteil unetr allen 12 Monate
dat %>% 
  filter((streik_ztp == 1) & taetigk %in% 1:3) %>% 
  group_by(gewerk) %>% 
  summarise(n = sum(gewicht)) %>% 
  mutate(p=n/sum(n))

# Institutionenvertrauen

dat %>% 
  filter(taetigk %in% 1:3 & Welle == 9) %>% 
  select(contains("instv"), gewicht) %>% 
  pivot_longer(-gewicht) %>% 
  mutate(x = case_when(value %in% 4:5 ~ "Großes oder sehr großes Vertrauen",
            value == 3 ~ "Mittleres Vertrauen",
            value %in% 1:2 ~ "Wenig oder überhaupt kein Vertrauen")) %>% 
  filter(!is.na(x)) %>% 
  group_by(name, x) %>% 
  summarise(n = sum(gewicht)) %>% 
  mutate(p=round(n/sum(n)*100)) %>% 
  pivot_wider(id_cols=name, names_from=x, values_from =p) %>% 
  write.csv2("output/instv.csv")
