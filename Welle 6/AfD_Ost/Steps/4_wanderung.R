dat %>%
  filter(W6_2_w6 < 96) %>%
  mutate(partei_6 = as_factor(W6_2_w6), partei_5 = as_factor(partei_5)) %>%
  group_by(partei_5, partei_6) %>%
  summarise(n = sum(Faktor_w123456), N=n()) %>%
  filter(partei_6 %in% levels(partei_6)[1:7]) %>%
  mutate(p = round(n/sum(n)*100), N=sum(N)) %>%
  pivot_wider(names_from=partei_6, values_from=p, id_cols=c(partei_5, N)) %>%
  filter(partei_5 %in% levels(partei_5)[1:6]) %>%
  mutate(`CDU/CSU` = CDU+CSU) %>%
  select(partei_5, N, `CDU/CSU`, SPD, AfD, FDP, `Die Linke`, `Bündnis 90 / Die Grünen`) %>%
  write.csv2("Output/wählerwanderung.csv")
  
