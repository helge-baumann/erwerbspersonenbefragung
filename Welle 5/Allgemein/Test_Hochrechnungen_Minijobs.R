dat %>% 
  filter(SC1 == 1 & is.na(Aufstockung) & S6 %in% 1:2) %>% 
  mutate(gewicht_basis = Faktor2 * 38000000/sum(Faktor2)) %>% 
  group_by(D6_6) %>% 
  summarise(hochrechnung = sum(gewicht_basis))
