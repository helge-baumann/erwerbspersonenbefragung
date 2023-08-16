# Zeitreihe Homeoffice
# 31.08.2022
# Helge Emmler
# R 4.2.1
library(pacman)
p_load(haven, dplyr, tidyr)

dat <- read_dta("Erwerbspersonenbefragung_Wellen1-8_long_full_v1-1.dta")
dir.create("./Output")

dat$S11_2o_w34578[is.na(dat$S11_2o_w34578)] <- 0
dat$S11_3o_w34578[is.na(dat$S11_3o_w34578)] <- 0

dat %>% 
  filter(Welle == 8) %>% 
  select(W8_Z1_w8, 
         Faktor_voll_w578, 
         S2_w12345678,
         S3_w12345678,
         D2_w3578,
         S11_2o_w34578, S11_3o_w34578) %>% 
  mutate(
    `1. Gesamt`="1. Gesamt",
    Geschlecht = as_factor(S2_w12345678), Bildung = as_factor(S3_w12345678),
    `Kinder im Haushalt` = if_else(S11_2o_w34578 > 1 | S11_3o_w34578 > 1, "Ja", "Nein"),
    Individualnettoeinkommen = case_when(
      D2_w3578 %in% 1:4 ~ "bis unter 1.500 Euro",
      D2_w3578 %in% 5:7 ~ "1.500 bis unter 2.600 Euro",
      D2_w3578 %in% 8:9 ~ "2.600 bis unter 3.800 Euro",
      D2_w3578 %in% 10:12 ~ "3.800 Euro und mehr",
      D2_w3578 %in% 99 ~ "keine Angabe"
    )
  ) %>% 
  pivot_longer(
    cols = c(`1. Gesamt`, Geschlecht, Bildung, Individualnettoeinkommen, `Kinder im Haushalt`),
    names_to = "var", values_to = "val"
    ) %>% 
  group_by(var, val, W8_Z1_w8) %>% 
  filter(!is.na(W8_Z1_w8) & !is.na(val)) %>% 
  summarise(n = sum(Faktor_voll_w578), N = n()) %>% 
  mutate( kA = sum(N[W8_Z1_w8 == 99]), N=sum(N)-kA) %>% 
  filter(W8_Z1_w8 < 99) %>% 
  mutate(p = round(n/sum(n)*100), W8_Z1_w8 = as_factor(W8_Z1_w8)) %>% 
  pivot_wider(id_cols=c(var, val, N, kA), names_from=W8_Z1_w8, values_from=p) %>% 
  write.csv2(paste0("Output/", Sys.Date(), "_Rente-bis-67.csv"))
  

          