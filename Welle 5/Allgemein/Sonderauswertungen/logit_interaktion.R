dat_glm <- dat %>% 
  mutate(ho_2 = if_else(A2__2 %in% c(2,3), 1, 0)) %>%
  filter(D8__2 %in% c(1,2) & D9__2 %in% c(1,2)) %>% 
  select(ho_2, D9__2, D8__2, S6__2, Faktor__2) %>% 
  mutate_all(as_factor)

erg <- glm(ho_2 ~ S6__2 + D8__2 + D9__2 + D8__2:D9__2 , 
           data=dat_glm, family=binomial)

konst00 <- data.frame(S6__2 = "Angestellte/r", D8__2 = "Nein", D9__2 = "Nein")
konst10 <- data.frame(S6__2 = "Angestellte/r", D8__2 = "Ja", D9__2 = "Nein")
konst01 <- data.frame(S6__2 = "Angestellte/r", D8__2 = "Nein", D9__2 = "Ja")
konst11 <- data.frame(S6__2 = "Angestellte/r", D8__2 = "Ja", D9__2 = "Ja")

predict(erg, newdata=konst00, type="response")
predict(erg, newdata=konst10, type="response")
predict(erg, newdata=konst01, type="response")
predict(erg, newdata=konst11, type="response")

# Welle 4
dat_glm4 <- dat %>% 
  mutate(ho_4 = if_else(A2__4 %in% c(2,3), 1, 0)) %>%
  filter(D8__2 %in% c(1,2) & D9__2 %in% c(1,2)) %>% 
  select(ho_2, D9__2, D8__2, S6__2, Faktor__2) %>% 
  mutate_all(as_factor)

erg <- glm(ho_2 ~ S6__2 + D8__2 + D9__2 + D8__2:D9__2 , 
           data=dat_glm, family=binomial)