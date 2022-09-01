# WSI Report Nr. 65, April 2021, Seite 9, Abbildung 2 (Nur Zeile 2 öD)
dat_full %>% 
  filter(S6__1 < 4 & S6__2 < 4 & S6__3 < 4 & S6__4 < 4 & S6__5 < 5) %>%
  dplyr_relfreq2(S5__5, A2__5, Faktor2__5, round=F) # 35%

# WSI Report Nr. 65, April 2021, Seite 13, Abbildung 5
dat_full %>% 
  mutate(
    homeoffice = case_when(
      F2__1 %in% 2:3 & A2__2 %in% 2:3 & A2__3 %in% 2:3 & A2__4 %in% 2:3  & A2__5 %in% 2:3 ~ 1,
      F2__1 %in% 1 & A2__2 %in% 1 & A2__3 == 1 & A2__4 == 1 & A2__5 == 1 ~ 0)
    ) %>%
  filter(S6__1 < 4 & S6__2 < 4 & S6__3 < 4 & S6__4 < 4 & S6__5 < 4) %>%
  filter(!is.na(S1__2) & !is.na(S1__3) & !is.na(S1__4) & !is.na(S1__5)) %>%
  filter(A2s_1__5 != 99) %>%
  dplyr_relfreq2(homeoffice, A2s_1__5, Faktor2__5, round=F) # 29 zu 20

# WSI Policy Brief Nr. 52, 3/2021, Seite 16, Abbildung 11
  # nicht neu erhoben

# -	WSI Report Nr. 54, Januar 2020, Seite 9, Abbildung 3
  # nicht aus unserer Befragung

# -	WSI Report Nr. 54, Januar 2020, Seite 10, Abbildung 4
  # nicht aus unserer Befragung


  