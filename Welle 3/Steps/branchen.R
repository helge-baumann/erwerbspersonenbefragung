w1 <- dat %>%
  filter(!is.na(F2__1) & !is.na(A2__3)) %>%
  mutate(branche = as_factor(S5__3),
         home = as_factor(F2__1)) %>%
  group_by(home) %>%
  summarise(anteil = sum(Faktor__3), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n))

w3 <- dat %>%
  filter(!is.na(F2__1) & !is.na(A2__3)) %>%
  mutate(branche = as_factor(S5__3),
         home = as_factor(A2__3)) %>%
  group_by(branche, home) %>%
  summarise(anteil = sum(Faktor__3), n=n()) %>%
  mutate(anteil=round((anteil/sum(anteil)*100)), n=sum(n))

write.csv2(w1, "./Output/Branche_april.csv")
write.csv2(w3, "./Output/Branche_nov.csv")

dat <- read.xlsx(
  "./Output/20210119_Branche_AprNov.xlsx",
  sheet=1,
  startRow=2
) %>%
  select(-N)

dat <- pivot_longer(dat, -c(Branche, Antwort.Homeoffice), names_to="Monat", values_to="Wert")

dat$Wert <- round(dat$Wert, digits=0)

ggplot(dat, aes(x=Monat, y=Wert, fill=Antwort.Homeoffice, label=Wert)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  facet_wrap(~ Branche, scales="free", ncol=3) +
  scale_fill_manual(name= "", position = "bottom", 
                    values = c("Ich arbeite ausschließlich / überwiegend in meinem Betrieb." = "#2B5F65",
                               "Ich arbeite an wechselnden Arbeitsorten (Betrieb, zu Hause, mobil von unterwegs)." = "#E5F2F3", 
                               "Ich arbeite ausschließlich / überwiegend von zu Hause aus."="#698D92",
                               "Trifft nicht auf mich zu" = "#A6BFC1")) +
  #coord_flip() +
  theme(legend.position="top") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  guides(
    fill = guide_legend(ncol=2, title = NULL, byrow=T)) +
  labs(y="", x="")
