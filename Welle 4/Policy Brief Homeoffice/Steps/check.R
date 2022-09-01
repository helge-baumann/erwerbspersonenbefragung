dat_ges %>% 
  filter(A2__2 %in% c(2,3) & A2__3 %in% c(2,3))%>% 
  mutate(weiter = case_when(A2d__3 %in% 1 ~ 1, A2d__3 %in% 2:3 ~ 0)) %>%
  dplyr_relfreq2(weiter, A2b_3__3, Faktor__3)
