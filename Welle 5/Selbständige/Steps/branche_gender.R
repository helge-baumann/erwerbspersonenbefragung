write.csv2(
  dat5 %>%
  group_by(selbst, as_factor(S2), D1) %>%
  summarise(sw=sum(Faktor, na.rm=T), n=sum(!is.na(selbst_solo))) %>%
    mutate(p = sw/sum(sw)) %>%
    filter(D1 == 1), 
  "Output/gender.csv"
)

write.csv2(
  dat5 %>%
    group_by(selbst, as_factor(S5), D1) %>%
    summarise(sw=sum(Faktor, na.rm=T), n=sum(!is.na(selbst))) %>%
    mutate(p = sw/sum(sw)) %>%
    filter(D1 == 1), 
  "Output/branchen.csv"
)

write.csv2(
  dat5 %>%
    group_by(selbst, as_factor(S2), as_factor(S5)) %>%
    summarise(sw=sum(Faktor, na.rm=T), n=sum(!is.na(selbst))) %>%
    mutate(p = sw/sum(sw)),
  "Output/gender_branchen.csv"
)
    
