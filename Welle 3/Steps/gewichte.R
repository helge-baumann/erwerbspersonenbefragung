gew <- dat %>% 
  select(Faktor__1, Faktor__2, Faktor__3) %>% 
  mutate(
    n1 = sum(!is.na(Faktor__1)), 
    n2 = sum(!is.na(Faktor__2)), 
    n3 = sum(!is.na(Faktor__3)),
    ng = sum(!is.na(Faktor__1) & !is.na(Faktor__2) & !is.na(Faktor__3)),
    n13 = sum(!is.na(Faktor__1) & !is.na(Faktor__3))
    ) %>%
  mutate(
    norm2 = Faktor__2 * (n1/n2),
    norm2_n = sum(norm2, na.rm=T),
    norm3 = Faktor__3 * n3
  )

