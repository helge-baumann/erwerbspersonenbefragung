write.csv2(dat %>%
  filter(!is.na(gewerkschaft)) %>%
  pivot_longer(cols=c(`(1) gesamt`:erwerbsstatus, zufriedenheit_regierung:alter), 
               names_to="Variable", values_to="Wert") %>%
  group_by(gewerkschaft, Variable, Wert) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)),
  "Output/gewerkschaft_check.csv")

write.csv2(dat %>%
  mutate(afd = 
           case_when(
             W6_2_w6 == 7 ~ "AfD", 
             W6_2_w6 %in% c(1:6, 8:14) ~ "andere Partei"
           )) %>%
  filter(!is.na(afd)) %>%
  pivot_longer(cols=c(`(1) gesamt`, bildung, geschlecht), names_to="Variable", values_to="Wert") %>%
  group_by(Variable, Wert, gewerkschaft, afd) %>%
  summarise(n = sum(Faktor_w123456), N = n()) %>%
  mutate(p = round(n/sum(n)*100, digits=1), N = sum(N)) %>%
    ungroup() %>%
    filter(afd == "AfD") %>%
    filter(!is.na(Wert) & N > 100) %>%
  pivot_wider(names_from = c(gewerkschaft), values_from = p, 
              id_cols=c(Variable, Wert)),
  "Output/afd_gewerkschaft.csv")

