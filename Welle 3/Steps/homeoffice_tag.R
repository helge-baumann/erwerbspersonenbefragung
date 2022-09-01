plotdata <- dat3 %>% 
  select(Faktor, Interviewtag, A2) %>% 
  mutate(A2 = as_factor(A2),
         feldzeit = cut(Interviewtag,
                        breaks=seq(unique(dat3$Interviewtag)[1]-2, unique(dat3$Interviewtag)[19]+4, by=5),
                        labels=c("bis 08.11.", "bis 13.11.", "bis 18.11.", "nach 18.11."))) %>% 
  filter(!is.na(A2) & !is.na(feldzeit)) %>% 
  group_by(feldzeit, A2) %>%
  summarise(anteil = sum(Faktor), n=n()) %>%
  mutate(anteil=anteil/sum(anteil), n=sum(n))

plotdata %>%
  filter(A2 == levels(A2)[2]) %>%
ggplot() +
  geom_col(aes(x=feldzeit, y=anteil))

# Homeoffice nach Branchen
home <- dat %>%
  filter(!is.na(S5__3) & !(S5__3 %in% c(1,11,14))) %>%
  mutate(home = as_factor(A2__3), branche = as_factor(S5__3)) %>%
  group_by(branche, home) %>%
  summarise(anteil = sum(Faktor__3), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n))

write.csv2(home, "./Output/homebranche.csv")

homebg <- dat %>%
  filter(D7__3 < 99) %>%
  mutate(home = as_factor(A2__3), bg = as_factor(D7__3)) %>%
  group_by(bg, home) %>%
  summarise(anteil = sum(Faktor__3), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n))

write.csv2(homebg, "./Output/homebg.csv")

homeges <- dat %>%
  #filter(!is.na(S5__3) & !(S5__3 %in% c(1,11,14))) %>%
  mutate(home = as_factor(A2__3), branche = as_factor(S5__3)) %>%
  group_by(home) %>%
  summarise(anteil = sum(Faktor__3), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n))

write.csv2(homeges, "./Output/homeges.csv")


  


