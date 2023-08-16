dat %>% 
  group_by(geschlecht, vtw) %>% 
  filter(!is.na(geschlecht) & !is.na(vtw) & vtw > 0 &azv_grg == 1 & az_mp_okt22 >= 35 & az_mp_okt22 <= 60 &  alter <= 65) %>% 
  summarise(N = n(), n = sum(gewicht, na.rm=T)) %>% 
  mutate(N = sum(N), p=n/sum(n))

dat %>% 
  filter(geschlecht %in% 1:2  & azv_grg != 2  & alter <= 65 & w5_erwstat_mp_tz == 2 & w5_erwstat_mp_sv == 1) %>% 
  group_by(geschlecht, vtw) %>% 
  summarise(N = n(), n = sum(gewicht_alle, na.rm=T)) %>% 
  mutate(N = sum(N), p=n/sum(n)) # 45/41 # 46/41


dat %>% 
  group_by(vtw) %>% 
  filter(geschlecht %in% 1:2 & !is.na(vtw) & vtw > 0 &azv_grg == 1 & az_mp_okt22 >= 35 & az_mp_okt22 <= 60 &  alter <= 65) %>% 
  summarise(N = n(), n = sum(gewicht, na.rm=T)) %>% 
  mutate(N = sum(N), p=n/sum(n))

# Kinder wie definiert?
dat %>% 
  group_by(geschlecht, vtw) %>% 
  filter(!is.na(geschlecht) & !is.na(vtw) & vtw > 0 &azv_grg == 1 & az_mp_okt22 >= 35 & az_mp_okt22 <= 60 &  alter <= 65 &
           w9_kinder == 2) %>% 
  summarise(N = n(), n = sum(gewicht, na.rm=T)) %>% 
  mutate(N = sum(N), p=n/sum(n))

dat %>% 
  mutate(alter_g = case_when(alter >= 21 & alter <= 30 ~ 0, alter > 30 & alter <= 65 ~ 1)) %>% 
  group_by(alter_g, vtw) %>% 
  filter(!is.na(geschlecht) & !is.na(vtw) & vtw > 0 &azv_grg == 1 & az_mp_okt22 >= 35 & az_mp_okt22 <= 60 &  alter <= 65) %>% 
  summarise(N = n(), n = sum(gewicht, na.rm=T)) %>% 
  mutate(N = sum(N), p=n/sum(n))
