# Additive Verluste

cum.na <- function(x) {
  x[which(is.na(x))] <- 0
  return(cumsum(x))
}

dat_long <- dat_long %>%
  group_by(lfdn_w1__1) %>%
  filter(Welle != 4) %>%
  arrange(lfdn_w1__1, Welle) %>%
  mutate(
    v_i = case_when(
      F8_w1235 == 1 ~ 1,
      F8_w1235 %in% 2:3 ~ 0,
      F8_w1235 == 99 ~ NA_real_,
      is.na(F8_w1235) ~ NA_real_
    ),
    v_hh = case_when(
      F9_w1235 == 1 ~ 1,
      F9_w1235 %in% 2:3 ~ 0,
      F9_w1235 == 99 ~ NA_real_,
      is.na(F9_w1235) ~ NA_real_
    ),
    v_hh = if_else(F9_w1235 == 4, v_i, v_hh),
    add_i = cum.na(v_i), add_i = if_else(cumsum(is.na(v_i)) > 0 & add_i == 0, NA_integer_, as.integer(add_i)),
    add_hh = cum.na(v_hh), add_hh = if_else(cumsum(is.na(v_hh)) > 0 & add_hh == 0, NA_integer_, as.integer(add_hh)),
    i0 = W5_D2a_w5[4],
    hh0 = W5_D4a_w5[4],
    i0_g = case_when(
      i0 %in% c(1:3) ~ "bis unter 1.300 Euro",
      i0 %in% c(4:6) ~ "1.300 bis unter 2.000 Euro",
      i0 %in% c(7:8) ~ "2.000 bis unter 3.200 Euro",
      i0 %in% c(9:10) ~ "3.200 bis unter 4.500 Euro",
      i0 %in% c(11:12) ~ "4.500 bis unter 6.000 Euro"
    ),
    hh0_g = case_when(
      hh0 %in% c(1:6) ~ "bis unter 2.000 Euro",
      hh0 %in% c(7:8) ~ "2.000 bis unter 3.200 Euro",
      hh0 %in% c(9:10) ~ "3.200 bis unter 4.500 Euro",
      hh0 %in% c(11:12) ~ "4.500 und mehr"
    )
    )

dat_long <- dat_long %>%
  mutate(check_i = cumsum(is.na(v_i)),
         check_i = if_else(Welle == 5, check_i-1, as.double(check_i)), 
         check_hh = cumsum(is.na(v_hh)),
         check_hh = if_else(Welle == 5, check_hh-1, as.double(check_hh)), 
         teilnahme = sum(
           Welle == 1 & !is.na(S1_w12345),
           Welle == 2 & !is.na(S1_w12345),
           Welle == 3 & !is.na(S1_w12345),
           Welle == 5 & !is.na(S1_w12345)),
         gew_w5 = Faktor_w12345[4])

Verluste <- data.frame(
  unbalanced_single_i_p = integer(4),  unbalanced_single_i_N = integer(4),
  unbalanced_single_hh_p = integer(4),  unbalanced_single_hh_N = integer(4),
  unbalanced_add_i_p = integer(4),  unbalanced_add_i_N = integer(4),
  unbalanced_add_hh_p = integer(4),  unbalanced_add_i_N = integer(4),
  balanced_single_i_p = integer(4),  balanced_single_i_N = integer(4),
  balanced_single_hh_p = integer(4),  balanced_single_hh_N = integer(4),
  balanced_add_i_p = integer(4),  balanced_add_i_N = integer(4),
  balanced_add_hh_p = integer(4),  balanced_add_i_N = integer(4)
) %>%
  tibble()

# unbalanced, single
Verluste[,1:2] <- dat_long %>% 
  dplyr_relfreq2(Welle, v_i, Faktor_w12345, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

Verluste[,3:4] <- dat_long %>% 
  dplyr_relfreq2(Welle, v_hh, Faktor_w12345, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

# unbalanced, add
Verluste[,5:6] <- dat_long %>% 
  mutate(add_i = if_else(add_i == 0, 0, 1)) %>%
  filter(!is.na(S1_w12345)) %>%
  dplyr_relfreq2(Welle, add_i, Faktor_w12345, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

Verluste[,7:8] <- dat_long %>% 
  mutate(add_hh = if_else(add_hh == 0, 0, 1)) %>%
  filter(!is.na(S1_w12345)) %>%
  dplyr_relfreq2(Welle, add_hh, Faktor_w12345, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

# balanced, single
Verluste[,9:10] <- dat_long %>% 
  filter(teilnahme == 4) %>%
  filter(!is.na(gew_w5)) %>%
  dplyr_relfreq2(Welle, v_i, gew_w5, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

Verluste[,11:12] <- dat_long %>% 
  filter(teilnahme == 4) %>%
  dplyr_relfreq2(Welle, v_hh, gew_w5, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

# balanced, add
Verluste[,13:14] <- dat_long %>% 
  mutate(add_i = if_else(add_i == 0, 0, 1)) %>%
  filter(teilnahme == 4) %>%
  filter(!is.na(S1_w12345)) %>%
  dplyr_relfreq2(Welle, add_i, gew_w5, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

Verluste[,15:16] <- dat_long %>% 
  mutate(add_hh = if_else(add_hh == 0, 0, 1)) %>%
  filter(teilnahme == 4) %>%
  filter(!is.na(S1_w12345)) %>%
  dplyr_relfreq2(Welle, add_hh, gew_w5, round=F) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

write.csv2(Verluste, "./Output/einbussen.csv")

# Haushaltseinkommen
HH_verluste <- data.frame(
  w1_p = integer(4),
  w1_n = integer(4),
  w2_p = integer(4),
  w2_n = integer(4),
  w3_p = integer(4),
  w3_n = integer(4),
  w5_p = integer(4),
  w5_n = integer(4)
) %>% tibble()

num <- 1
for(i in c(1,2,3,5)) {
  
  
HH_verluste[num:(num+1)] <- dat_long %>% 
  #mutate(Welle_neu = if_else(Welle == 5, 4, Welle)) %>%
  mutate(add_hh = if_else(add_hh == 0, 0, 1)) %>%
  #filter(check_i < Welle_neu) %>% 
  filter(!is.na(S1_w12345) & Welle == i) %>%
  dplyr_relfreq2(hh0_g, add_hh, Faktor_w12345) %>%
  filter(y == 1) %>% ungroup() %>% select(anteil, n)

num <- num+2

}

write.csv2(HH_verluste, "./Output/HH_verluste.csv")

I_verluste <- data.frame(
  w1_p = integer(5),
  w1_n = integer(5),
  w2_p = integer(5),
  w2_n = integer(5),
  w3_p = integer(5),
  w3_n = integer(5),
  w5_p = integer(5),
  w5_n = integer(5)
) %>% tibble()

num <- 1
for(i in c(1,2,3,5)) {
  
  
  I_verluste[num:(num+1)] <- dat_long %>% 
    #mutate(Welle_neu = if_else(Welle == 5, 4, Welle)) %>%
    mutate(add_i = if_else(add_i == 0, 0, 1)) %>%
    #filter(check_i < Welle_neu) %>% 
    filter(!is.na(S1_w12345) & Welle == i) %>%
    dplyr_relfreq2(i0_g, add_i, Faktor_w12345) %>%
    filter(y == 1) %>% ungroup() %>% select(anteil, n)
  
  num <- num+2
  
}

write.csv2(I_verluste, "./Output/I_verluste.csv")



