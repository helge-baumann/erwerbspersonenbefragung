dat$w5_phh_mp_ue17[dat$Welle == 9] <- dat$w5_phh_mp_ue17[dat$Welle == 9]-1
dat$w5_phh_mp_ue13u18[dat$Welle == 9] <- dat$w5_phh_mp_ue13u18[dat$Welle == 9]-1
dat$w5_phh_mp_u14[dat$Welle == 9] <- dat$w5_phh_mp_u14[dat$Welle == 9]-1

dat %>% filter(Welle %in% 8:9) %>%  group_by(Welle) %>%  
  mutate(
    w5_phh_mp_u14_adp = case_when(
      !is.na(w5_phh_mp_u14) & !is.na(geschlecht) ~ w5_phh_mp_u14, 
      is.na(w5_phh_mp_u14) & !is.na(geschlecht) ~ 0
    ),
    w5_phh_mp_ue13u18_adp = case_when(
      !is.na(w5_phh_mp_ue13u18) & !is.na(geschlecht) ~ w5_phh_mp_ue13u18, 
      is.na(w5_phh_mp_ue13u18) & !is.na(geschlecht) ~ 0
    ),
    w5_phh_mp_ue17_adp = case_when(
      !is.na(w5_phh_mp_ue17) & !is.na(geschlecht) ~ w5_phh_mp_ue17, 
      is.na(w5_phh_mp_ue17) & !is.na(geschlecht) ~ 1
    ),
    oecd_alt = if_else(
      !is.na(w5_phh_mp_u14) & !is.na(w5_phh_mp_ue13u18) & !is.na(w5_phh_mp_ue17) &Welle %in% 8:9, 
      0.5+(w5_phh_mp_ue13u18+w5_phh_mp_ue17)*0.5+(w5_phh_mp_u14)*0.3, NA_real_
    ), 
    oecd_neu = if_else(
      !is.na(w5_phh_mp_u14_adp) & !is.na(w5_phh_mp_ue13u18_adp) & !is.na(w5_phh_mp_ue17_adp) &Welle %in% 8:9, 
      0.5+(w5_phh_mp_ue13u18_adp+w5_phh_mp_ue17_adp)*0.5+(w5_phh_mp_u14_adp)*0.3, NA_real_
    ),
    hhinc_num = case_when(
      w3_hh_income == 1 ~ 400,  w3_hh_income == 2 ~ 700,  w3_hh_income == 3 ~ 1100,
      w3_hh_income == 4 ~ 1400,  w3_hh_income == 5 ~ 1600,  w3_hh_income == 6 ~ 1850,
      w3_hh_income == 7 ~ 2300,  w3_hh_income == 8 ~ 2900,  w3_hh_income == 9 ~ 3500,
      w3_hh_income == 10 ~ 4150,  w3_hh_income == 11 ~ 5250,  w3_hh_income == 12 ~ 6500
    ),
    `Äquivalenzeinkommen_alt` = hhinc_num/oecd_alt,
    `Äquivalenzeinkommen_neu` = hhinc_num/oecd_neu,
    
  ) %>% 
  summarise(U14 = mean(w5_phh_mp_u14, na.rm=T),
            `14 bis 17` = mean(w5_phh_mp_ue13u18, na.rm=T),
            `18+` = mean(w5_phh_mp_ue17, na.rm=T),
            Haushaltsgröße = mean(w5_phh_mp_u14+w5_phh_mp_ue13u18+w5_phh_mp_ue17, na.rm=T),
            Äqui_alt_med = median(`Äquivalenzeinkommen_alt`, na.rm=T),
            Äqui_alt_mean = mean(`Äquivalenzeinkommen_alt`, na.rm=T),
            U14_neu = mean(w5_phh_mp_u14_adp, na.rm=T),
            `14 bis 17 neu` = mean(w5_phh_mp_ue13u18_adp, na.rm=T),
            `18+ neu` = mean(w5_phh_mp_ue17_adp, na.rm=T),
            Haushaltsgröße_neu = mean(w5_phh_mp_u14_adp+w5_phh_mp_ue13u18_adp+w5_phh_mp_ue17_adp, na.rm=T),
            Äqui_neu_med = median(`Äquivalenzeinkommen_neu`, na.rm=T),
            Äqui_neu_mean = mean(`Äquivalenzeinkommen_neu`, na.rm=T)
            ) %>% 
  write.csv2("hh_eink.csv")

