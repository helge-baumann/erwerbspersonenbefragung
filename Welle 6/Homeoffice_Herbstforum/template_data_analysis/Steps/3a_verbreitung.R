write.csv2(dat %>%
  pivot_longer(cols=`2020-01-15`:`2021-07-01`, names_to="var", values_to="value") %>%
  group_by(var, value) %>%
  filter(!is.na(value) & !is.na(Faktor_w123456)) %>%
  summarise(n=sum(Faktor_w123456)) %>%
  mutate(p = n/sum(n)) %>%
  filter(value == 1) %>%
  mutate(var=as.Date(var)),
  "Output/Verbreitung.csv")

