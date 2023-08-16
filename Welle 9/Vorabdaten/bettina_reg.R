# Replikation Bettina

dat %>%  
  filter(
    Welle == 2 & 
      azvor_std <= 60 & 
      az_mp_mrz20 <= 60 &
      az_mp_apr20 <= 60 &
      az_mp_mai20 <= 60 &
      az_mp_jun20 <= 60) %>% 
  select(Welle, azvor_std, az_mp_mrz20, az_mp_apr20, az_mp_mai20, az_mp_jun20, gewicht) %>% 
  pivot_longer(cols=starts_with("az_mp"), names_to="arbz", values_to="value") %>% 
  
  group_by(arbz) %>% 
  summarise(
    mean_vor=wtd.mean(azvor_std, gewicht, na.rm=T),
    lost_std = wtd.mean(azvor_std - value, gewicht, na.rm=T),
    lost_dev = sqrt(wtd.var(azvor_std - value, gewicht, na.rm=T))
    )  # 36.51 statt 36.52

dat %>%  
  filter(
    Welle %in% 1:2 #& 
     # azvor_std <= 60 & 
     # az_mp_mrz20 <= 60 &
      #az_mp_apr20 <= 60 &
      #az_mp_mai20 <= 60 &
      #az_mp_jun20 <= 60
    ) %>% 
  select(Welle, azvor_std, az_mp_mrz20, az_mp_apr20, az_mp_mai20, az_mp_jun20, gewicht, kinder, azrk_mp_ich) %>% 
  pivot_longer(cols=starts_with("az_mp"), names_to="arbz", values_to="value") %>% 
  group_by(kinder, azrk_mp_ich) %>% 
  filter(kinder %in% 1:2) %>% 
  summarise(
    n=sum(gewicht, na.rm=T)
  ) %>%  mutate(p=n/sum(n))  # 36.51 statt 36.52
