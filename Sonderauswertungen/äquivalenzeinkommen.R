# Mittelwerte

dat_long_full %>%
  ungroup() %>%
  filter(Welle == 5 & !is.na(Faktor2_w5)) %>%
  mutate(
    hh18 = if_else(is.na(S11_1_w5), 0, S11_1_w5),
    hh1418 = if_else(is.na(S11_2o_w345), 0, as.numeric(S11_2o_w345)),
    hh14 = if_else(is.na(S11_3o_w345), 0, as.numeric(S11_3o_w345)),
    oecd_weight = 1+(hh_ue18_w12345+hh_ue14u18_w12345-1)*0.5+hh_u14_w12345*0.3,
    oecd_weight_alt = 1+(hh18 + hh1418-1)*0.5 + hh14*0.3,
    D4_w35 = if_else(D4_w35 == 99 & D2_w35 %in% 1:12, D2_w35, D4_w35),
    W5_D4a_w5 = if_else(W5_D4a_w5 == 99 & W5_D2a_w5 %in% 1:12, W5_D2a_w5, W5_D4a_w5),
    hh_income = case_when(
      D4_w35 == 1 ~ 0, D4_w35 == 2 ~ 700, D4_w35 == 3 ~ 1100, 
      D4_w35 == 4 ~ 1400, D4_w35 == 5 ~ 1600, D4_w35 == 6 ~ 1850,
      D4_w35 == 7 ~ 2300, D4_w35 == 8 ~ 2900, D4_w35 == 9 ~ 3500,
      D4_w35 == 10 ~ 4150, D4_w35 == 11 ~ 5250, D4_w35 == 12 ~ 10000
    ),
    hh_income_vor = case_when(
      W5_D4a_w5 == 1 ~ 0, W5_D4a_w5 == 2 ~ 700, W5_D4a_w5 == 3 ~ 1100, 
      W5_D4a_w5 == 4 ~ 1400, W5_D4a_w5 == 5 ~ 1600, W5_D4a_w5 == 6 ~ 1850,
      W5_D4a_w5 == 7 ~ 2300, W5_D4a_w5 == 8 ~ 2900, W5_D4a_w5 == 9 ~ 3500,
      W5_D4a_w5 == 10 ~ 4150, W5_D4a_w5 == 11 ~ 5250, W5_D4a_w5 == 12 ~ 10000
    ),
    hh_income_w = hh_income/oecd_weight,
    hh_income_vor_w = hh_income_vor/oecd_weight,
    hh_income_w_alt = hh_income/oecd_weight_alt,
    hh_income_vor_w_alt = hh_income_vor/oecd_weight_alt
    ) %>%
  
  #ggplot() + geom_histogram(aes(x=hh_income_w))
  summarise(
    median_income = wtd.quantile(hh_income_w, probs=0.5, Faktor2_w5),
    n = sum(!is.na(hh_income_w)),
    median_income_vor = wtd.quantile(hh_income_vor_w, probs=0.5, Faktor2_w5),
    n2 = sum(!is.na(hh_income_vor_w)),
    median_income_alt = wtd.quantile(hh_income_w_alt, probs=0.5, Faktor2_w5),
    n3 = sum(!is.na(hh_income_w_alt)),
    median_income_vor_alt = wtd.quantile(hh_income_vor_w_alt, probs=0.5, Faktor2_w5),
    n4 = sum(!is.na(hh_income_vor_w_alt)),
    mean(oecd_weight_alt, na.rm=T), mean(oecd_weight, na.rm=T)
  )

# Äquivalenzklassen
dat_income <- dat_long_full %>%
  ungroup() %>%
  filter(Welle == 5 & !is.na(Faktor2_w5)) %>%
  mutate(
    hh18 = if_else(is.na(S11_1_w5), 0, S11_1_w5),
    hh1418 = if_else(is.na(S11_2o_w345), 0, as.numeric(S11_2o_w345)),
    hh14 = if_else(is.na(S11_3o_w345), 0, as.numeric(S11_3o_w345)),
    #oecd_weight = 1+(hh_ue18_w12345+hh_ue14u18_w12345-1)*0.5+hh_u14_w12345*0.3,
    oecd_weight = 1+(hh18 + hh1418-1)*0.5 + hh14*0.3,
    D4_w35 = if_else(D4_w35 == 99 & D2_w35 %in% 1:12, D2_w35, D4_w35),
    W5_D4a_w5 = if_else(W5_D4a_w5 == 99 & W5_D2a_w5 %in% 1:12, W5_D2a_w5, W5_D4a_w5),
    lower = case_when(
      W5_D4a_w5 == 1 ~ 0/oecd_weight, W5_D4a_w5 == 2 ~ 500/oecd_weight, 
      W5_D4a_w5 == 3 ~ 900/oecd_weight,W5_D4a_w5 == 4 ~ 1300/oecd_weight, 
      W5_D4a_w5 == 5 ~ 1500/oecd_weight, W5_D4a_w5 == 6 ~ 1700/oecd_weight,
      W5_D4a_w5 == 7 ~ 2000/oecd_weight, W5_D4a_w5 == 8 ~ 2600/oecd_weight, 
      W5_D4a_w5 == 9 ~ 3200/oecd_weight, W5_D4a_w5 == 10 ~ 3800/oecd_weight, 
      W5_D4a_w5 == 11 ~ 4500/oecd_weight, W5_D4a_w5 == 12 ~ 6000/oecd_weight
    ),
    upper = case_when(
      W5_D4a_w5 == 1 ~ 500/oecd_weight, W5_D4a_w5 == 2 ~ 900/oecd_weight, 
      W5_D4a_w5 == 3 ~ 1300/oecd_weight,W5_D4a_w5 == 4 ~ 1500/oecd_weight, 
      W5_D4a_w5 == 5 ~ 1700/oecd_weight, W5_D4a_w5 == 6 ~ 2000/oecd_weight,
      W5_D4a_w5 == 7 ~ 2600/oecd_weight, W5_D4a_w5 == 8 ~ 3200/oecd_weight, 
      W5_D4a_w5 == 9 ~ 3800/oecd_weight, W5_D4a_w5 == 10 ~ 4500/oecd_weight, 
      W5_D4a_w5 == 11 ~ 6000/oecd_weight, W5_D4a_w5 == 12 ~ 100000/oecd_weight
    ),
    aeq_class = paste0(lower, upper)
    
  ) %>%
  filter(!is.na(lower)) %>%
  arrange(aeq_class) %>%
  group_by(aeq_class) %>%
  mutate(size_class = n(), pos=row_number())
  
dat_income$wert <- NA

for(i in 1:nrow(dat_income)) {
  
  if(dat_income$size_class[i] == 1) {
dat_income$wert[i] <- 
  dat_income$lower[i]+
  (dat_income$upper[i]-
     dat_income$lower[i])/2
  } else {
  
    dat_income$wert[i] <- 
      seq(dat_income$lower[i], dat_income$upper[i], length.out=dat_income$size_class[i])[dat_income$pos[i]]
    
  }
  
}

wtd.quantile(dat_income$wert, dat_income$Faktor2_w5)
  
 

