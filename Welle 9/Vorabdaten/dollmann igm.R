dat %>% 
  filter(Welle %in% c(4,7,9)) %>% 
  group_by(Welle, home_potenzial) %>% 
  filter(!is.na(home_potenzial)) %>% 
  summarise(N = n(), n = sum(gewicht)) %>% 
  mutate(N = sum(N), p=round(n/sum(n)*100))
         
dat %>% 
  filter(Welle %in% c(4,7,9)) %>% 
  group_by(ID) %>% 
  mutate(n = sum(!is.na(home_potenzial))) %>% 
  filter( n == 3) %>% 
  group_by(Welle, home_potenzial) %>% 
  summarise(N = n(), n = sum(gewicht)) %>% 
  mutate(N = sum(N), p=round(n/sum(n)*100))
