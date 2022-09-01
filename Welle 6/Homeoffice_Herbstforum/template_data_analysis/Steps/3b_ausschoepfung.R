# Potenzial

write.csv2(dat %>% 
  filter(!is.na(potenzial_diff) & Welle == 4) %>%
  group_by(potenzial_diff) %>%
  summarise(n = sum(gewicht_w4)) %>%
  mutate(p = n/sum(n)),
  "Output/potenzial_diff.csv")

write.csv2(dat %>% 
  filter(!is.na(potenzial) & Welle == 4) %>%
  summarise(wtd.mean(potenzial, gewicht_w4)),
  "Output/potenzial.csv")

# Ausschöpfung Potenzial über die Zeit
write.csv2(
  dat %>%
  pivot_longer(cols=c(`betrieb_2020-01-15`:`betrieb_2021-07-01`), names_to="betrieb", values_to="value") %>% 
  filter(!is.na(value) & !is.na(potenzial)) %>%
  group_by(betrieb, potenzial, value) %>%
  summarise(n = sum(gewicht_w4)) %>%
  mutate(p = n/sum(n), betrieb=as.Date(str_remove_all(betrieb, "betrieb_"))) %>%
  mutate(konstellation = case_when(
    potenzial == 1 & value ==  0 ~ "Homeoffice-Potential, im Homeoffice",
    potenzial == 0 & value ==  0 ~ "kein Homeoffice-Potential, im Homeoffice",
  )) %>% 
  filter(!is.na(konstellation)),
  "Output/ausschoepfung_zeit.csv")

# Schichtungen
write.csv2(
  dat %>% 
  mutate(kinder = as_factor(case_when(hh_u14_w12345 == 0 ~ 0, hh_u14_w12345 >= 1 ~ 1))) %>%
  filter(!is.na(potenzial) & !is.na(S2_w123456) & Welle == 4) %>%
  pivot_longer(cols=c(sex, haushaltstyp, net_income, betriebsgroesse, branche, betriebsrat, tarifvertrag), names_to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert) %>%
  filter(!is.na(Wert)) %>%
  summarise(p=wtd.mean(potenzial, gewicht_w4), n=n()),
  "Output/Potenzial_schichtungen.csv"
)


# Ausschöpfung Potenzial über die Zeit
write.csv2(dat %>%
  mutate(kinder = as_factor(case_when(hh_u14_w12345 == 0 ~ 0, hh_u14_w12345 >= 1 ~ 1))) %>%
  pivot_longer(cols=c(`betrieb_2020-01-15`:`betrieb_2021-07-01`), names_to="betrieb", values_to="value") %>% 
  filter(!is.na(value) & !is.na(potenzial)) %>%
  pivot_longer(cols=c(sex, haushaltstyp, net_income, betriebsgroesse, branche, betriebsrat, tarifvertrag), names_to="Variable", values_to="Wert") %>%
  filter(!is.na(Wert) & Wert != "Keine Angabe") %>%
  group_by(betrieb, Variable, Wert, potenzial, value) %>%
  summarise(n = sum(gewicht_w4), N=n()) %>%
  mutate(p = n/sum(n), betrieb=as.Date(str_remove_all(betrieb, "betrieb_")), N=sum(N)) %>%
  mutate(konstellation = case_when(
    potenzial == 1 & value ==  0 ~ "Homeoffice-Potential, im Homeoffice"
  )) %>% 
  filter(!is.na(konstellation)),
  "Output/Ausschoepfung_schichtungen.csv")

