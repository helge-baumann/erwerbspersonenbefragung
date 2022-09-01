# Daten Welle 1-6 einlesen (Longformat)
dat <- read_dta("Input/Erwerbspersonenbefragung_Wellen1-6_long_full_v1-0.dta")

dat %>% group_by(ID) %>% 
  filter(is.na(Aufstockung_w5[5])) %>% 
  mutate(x = case_when(
    !is.na(S1_w123456[5]) & is.na(S1_w123456[6]) ~ "5, nicht 6", 
    !is.na(S1_w123456[5]) & !is.na(S1_w123456[6]) ~ "5 und 6", 
    is.na(S1_w123456[5]) & is.na(S1_w123456[6]) ~ "weder 5 noch 6", 
    is.na(S1_w123456[5]) & !is.na(S1_w123456[6]) ~ "nicht 5, aber 6", 
    )) %>%
  filter(Welle == 1) %>%
  ungroup %>% count(x)

write.csv2(dat %>%
  filter(Welle == 6 ) %>%
  mutate(partei = as_factor(W6_2_w6)) %>%
  pivot_longer(cols=c(W6_4a_1_w6:W6_4a_3_w6, W6_4a_5_w6), names_to = "var", values_to="value") %>%
  filter(!is.na(partei) & !is.na(value)) %>%
  mutate(value = case_when(value %in% 1 ~ "1", value %in% 2:3 ~ "2 bis 3", value >= 4 ~ "4 und mehr")) %>%
    group_by(partei, var, value) %>%
  summarise(n=sum(Faktor_w123456)) %>%
  mutate(p=n/sum(n)*100) %>%
  pivot_wider(names_from=var, values_from=p, id_cols=c(partei, value)), "Output/2und3.csv")
  