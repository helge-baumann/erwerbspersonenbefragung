dat_ges %>% dplyr_relfreq(W4_2__4, gew=Faktor__4)

pot_branche <- dat_ges %>% 
  mutate(pot = if_else(W4_2__4 %in% c(1,2), 1, 9)) %>%
  filter(!is.na(Faktor__4)) %>%
  dplyr_relfreq2(S5__4, pot, Faktor__4) %>%
  filter(y==1)

pot_branche$ho <- 
  dat_ges %>%
  mutate(ho = if_else(A2__4 %in% c(2,3), 1, 0)) %>%
  dplyr_relfreq2(S5__4, ho, Faktor__4) %>%
  filter(y==1) %>%
  pull(anteil)

pot_gk <- dat_ges %>% 
  mutate(pot = if_else(W4_2__4 %in% c(1,2), 1, 0)) %>%
  filter(!is.na(Faktor__4)) %>%
  dplyr_relfreq2(F32__1, pot, Faktor__4) %>%
  filter(y==1)

pot_gk$ho <- 
  dat_ges %>%
  mutate(ho = if_else(A2__4 %in% c(2,3), 1, 0)) %>%
  filter(!is.na(Faktor__4)) %>%
  dplyr_relfreq2(F32__1, ho, Faktor__4) %>%
  filter(y==1) %>%
  pull(anteil)

write.csv2(pot_branche, "./Output/Potenzial_Branche.csv")
write.csv2(pot_gk, "./Output/Potenzial_GK.csv")
  
