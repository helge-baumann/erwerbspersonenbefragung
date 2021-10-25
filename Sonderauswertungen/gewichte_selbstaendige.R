

dat_long %>%
  mutate(s = 
           case_when(S6_w45 %in% 1:3 ~ "unselbständig",
                     S6_w45 %in% 4:6 ~ "selbständig")) %>%
  group_by(Welle, s) %>%
  summarise(sum_of_weights=sum(Faktor_w12345)) %>%
  filter(!is.na(s)) %>%
  mutate(p = sum_of_weights/sum(sum_of_weights))

dat5 %>%
  mutate(s = 
           case_when(S6__5 %in% 1:3 ~ "unselbständig",
                     S6__5 %in% 4:6 ~ "selbständig")) %>%
  group_by(s) %>%
  summarise(sum_of_weights=sum(Faktor__5)) %>%
  filter(!is.na(s)) %>%
  mutate(p = sum_of_weights/sum(sum_of_weights))
