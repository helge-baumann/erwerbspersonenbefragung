dat_income <- dat5 %>%
  mutate(income_jetzt = case_when(
    D2 %in% 1:4 ~ "unter 1.500 Euro",
    D2 %in% 5:6 ~ "1.500 bis unter 2.000 Euro",
    D2 %in% 7:8 ~ "2.000 bis unter 3.200 Euro",
    D2 %in% 9:12 ~ "3.200 Euro und mehr"
  ),
  income_vor = case_when(
    W5_D2a %in% 1:4 ~ "unter 1.500 Euro",
    W5_D2a %in% 5:6 ~ "1.500 bis unter 2.000 Euro",
    W5_D2a %in% 7:8 ~ "2.000 bis unter 3.200 Euro",
    W5_D2a %in% 9:12 ~ "3.200 Euro und mehr"
  )) %>%
  select(starts_with("selbst"), income_vor, income_jetzt, Faktor, sex) %>%
  pivot_longer(cols=-c(income_vor, income_jetzt, "Faktor", sex), names_to="Variable", values_to="Wert") 

dat_income %>%
  group_by(Variable, Wert,  sex, income_jetzt) %>%
  summarise(n = sum(Faktor, na.rm=T)) %>%
  filter(!is.na(income_jetzt)) %>%
  mutate(p = n/sum(n)) %>%
  write.csv2("Output/Income_gender.csv")

dat_income %>%
  group_by(Variable, Wert, sex, income_vor) %>%
  summarise(n = sum(Faktor, na.rm=T)) %>%
  filter(!is.na(income_vor)) %>%
  mutate(p = n/sum(n)) %>%
  write.csv2("Output/Income_gender_vorher.csv")
  