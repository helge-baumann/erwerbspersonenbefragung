# Aus Welle 4: ----

# Nutzung von Homeoffice im Zeitverlauf

# Potenzial und Nutzung nach 
  # Branchen#
  # Betriebsgrößen

# Nutzung (Zeitverlauf)
  # M/W
  # Alter
  # Einkommen
  # Branvchen
  # Größen

# Homneoffice-Nutzung im Längsschnitt 
  # Branche
  # Größe

# Einstellungen
# Perspektive

# Interessant: ----

# Potenzial, Nutzung (auch home_tw), Ausschöpfung allgemein - abh.
# Zeit, Geschlecht, Einkommen - unabh. 


# Nutzung
long %>% 
  mutate(home = 
           case_when(
             home_akt == 1 | w8_home_akt == 1 ~ "Betrieb", 
             home_akt %in% 2:3 | w8_home_akt %in% 2:3 ~ "ganz / tw. zu Hause"
           )
         ) %>% 
  filter(!is.na(home)) %>% 
  group_by(Welle, home) %>% 
  summarise(n = sum(gewicht, na.rm=T)) %>% 
  mutate(p = round(n/sum(n)*100))

# Potenzial
long %>% 
  filter(!is.na(home_potenzial)) %>% 
  group_by(Welle, home_potenzial) %>% 
  summarise(n = sum(gewicht, na.rm=T)) %>% 
  mutate(p = round(n/sum(n)*100))

# Ausschöpfung
long %>% 
  mutate(
    Ausschöpfung =
      case_when(
        # kein potenzial und homeoffice = 0, 
        home_potenzial %in% 3:4 & (home_akt %in% 2:3 | w8_home_akt %in% 2:3) ~ 
          "0.1 kein Potenzial -> trotzdem Homeoffice",
        # Potenzial und Betrieb 
        home_potenzial %in% 1:2 & (home_akt == 1 | w8_home_akt == 1) ~ 
          "0.2 - Potenzial -> trotzdem Betrieb",
        # kein potenzial und betrieb = 2,
        home_potenzial %in% 3:4 & (home_akt == 1 | w8_home_akt == 1) ~ 
          "1.1 kein Potenzial -> korrekt im Betrieb",
        # potenzial und homeoffice = 3
        home_potenzial %in% 1:2 & (home_akt %in% 2:3 | w8_home_akt %in% 2:3) ~ 
          "1.2 Potenzial -> korrekt im Homeoffice"
        )
    ) %>% 
  filter(!is.na(Ausschöpfung)) %>% 
  group_by(Welle, Ausschöpfung) %>% 
  summarise(n = sum(gewicht, na.rm=T)) %>% 
  mutate(p = round(n/sum(n)*100))


# Äquivalenzeinkommen Nutzung Potenzial / Ausschöpfung; Zeitvergleich
# Männer / Frauen
# Branchen, evtl. BG
# Wie viel würden Sie gerne arbeiten? (Männer / Frauen; nach Potenzial; ab W9)
    # FRauen häufiger im Betrieb, wollen aber mehr von zu hause aus?

# Erstmal nur Welle 10
long %>% 
  mutate(
    Gesamt= "Gesamt",
    Geschlecht = as_factor(geschlecht),
    Wunsch = 
      case_when(
        # Wunsch 0:1, Homeoffice
        w9_home_persp %in% 0:1 & (home_akt %in% 2:3 | w8_home_akt %in% 2:3) ~ 
          "0.1 Wunsch: 0-1 Tage -> trotzdem nicht mehrheitlich im Betrieb",
        # Wunsch 0:1 Tage, Betrieb
        w9_home_persp %in% 0:1 & (home_akt %in% 1 | w8_home_akt %in% 1) ~ 
          "1.1 Wunsch: 0-1 Tage -> korrekt im Betrieb",
        # Wunsch 2:3, Homeoffice
        w9_home_persp %in% 2:3 & (home_akt %in% 2:3 | w8_home_akt %in% 2:3) ~ 
          "1.2 Wunsch: 2:3 Tage -> korrekt teilweise oder ganz im Homeoffice",
        w9_home_persp %in% 2:3 & (home_akt %in% 1 | w8_home_akt %in% 1) ~ 
          "0.2 Wunsch: 2:3 Tage -> trotzdem mehrheitlich im Betrieb",
        # Wunsch 4 und mehr Tage, Homeoffice
        w9_home_persp %in% 4:7 & (home_akt %in% 3 | w8_home_akt %in% 3) ~ 
          "1.3 Wunsch: 4 und mehr Tage -> korrekt mehrheitlich im Homeoffice",
        w9_home_persp %in% 4:7 & (home_akt %in% 1:2 | w8_home_akt %in% 1:2) ~ 
          "0.3 Wunsch: 4 und mehr Tage -> trotzdem mehrheitlich im Betrieb",
      ), 
    Ausschöpfung =
      case_when(
        # kein potenzial und homeoffice = 0, 
        home_potenzial %in% 3:4 & (home_akt %in% 2:3 | w8_home_akt %in% 2:3) ~ 
          "0.1 kein Potenzial -> trotzdem Homeoffice",
        # Potenzial und Betrieb 
        home_potenzial %in% 1:2 & (home_akt == 1 | w8_home_akt == 1) ~ 
          "0.2 - Potenzial -> trotzdem Betrieb",
        # kein potenzial und betrieb = 2,
        home_potenzial %in% 3:4 & (home_akt == 1 | w8_home_akt == 1) ~ 
          "1.1 kein Potenzial -> korrekt im Betrieb",
        # potenzial und homeoffice = 3
        home_potenzial %in% 1:2 & (home_akt %in% 2:3 | w8_home_akt %in% 2:3) ~ 
          "1.2 Potenzial -> korrekt im Homeoffice"
      ),
    Nettoäquivalenzeinkommen =
      cut(hh_inc_aeq_gen, 
          breaks= c(-1, 1500, 2500, 3500, 10000), 
          labels= c ("1. unter 1.500", "2. 1.500 bis 2.500", "3. 2.500 bis 3.500", "4. 3.500 und mehr")),
    w8_home_akt = as_factor(w8_home_akt), home_potenzial = as_factor(home_potenzial)
  ) %>% 
  pivot_longer(cols = c(Gesamt, Geschlecht, Nettoäquivalenzeinkommen), names_to ="UV", values_to = "UV_value") %>% 
  pivot_longer(cols = c(w8_home_akt, home_potenzial, Ausschöpfung, Wunsch), names_to ="AV", values_to = "AV_value") %>% 
    filter(Welle == 10 & !is.na(UV_value) & !is.na(AV_value) & az_mp_mai23 > 32) %>% 
    group_by(UV, UV_value, AV, AV_value) %>% 
    summarise(n = sum(gewicht, na.rm=T), N=n()) %>% 
    mutate(p = round(n/sum(n)*100), N=sum(N)) %>% 
  pivot_wider(id_cols=c(UV, UV_value), names_from=c(AV, AV_value), values_from=c(N, p)) %>% 
  write.csv2("output/w10.csv", fileEncoding="UTF-8")
  
