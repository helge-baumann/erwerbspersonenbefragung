erg <- data.frame(
  vor = dat_ges %>%
    dplyr_relfreq2(S2__1, F3__1, Faktor__1) %>% 
    filter(x %in% kat, y == "Ich habe an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs) gearbeitet.") %>%
    pull(anteil),
  apr = dat_ges %>% 
    dplyr_relfreq2(S2__1, F2__1, Faktor__1) %>% 
    filter(x %in% kat, y=="Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs).") %>%
    pull(anteil),
  jun = dat_ges %>% 
    dplyr_relfreq2(S2__1, A2__2, Faktor__2) %>% 
    filter(x %in% kat, y=="Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs).") %>%
    pull(anteil),
  nov = dat_ges %>% 
    dplyr_relfreq2(S2__1, A2__3, Faktor__3) %>% 
    filter(x %in% kat, y=="Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs).") %>%
    pull(anteil),
  dec = dat_ges %>% 
    dplyr_relfreq2(S2__1, W4_1__4, Faktor__4) %>% 
    filter(x %in% kat, y == "Ich habe an wechselnden Arbeitsorten gearbeitet (Betrieb, zu Hause, mobil von unterwegs)") %>%
    pull(anteil),
  jan = dat_ges %>% 
    dplyr_relfreq2(S2__1, A2__4, Faktor__4) %>% 
    filter(x %in% kat, y=="Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs).") %>%
    pull(anteil)
)
