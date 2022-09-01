library(ggplot2)

tage <- dat3 %>%
  group_by(Interviewtag) %>%
  count()

ggplot(tage) +
  geom_bar(
    aes(
      x=Interviewtag, 
      y=n), 
    stat="identity", 
    col="blue", 
    fill="blue") +
  #coord_flip() +
  scale_x_date(breaks=unique(tage$Interviewtag), labels=unique(tage$Interviewtag)) +
  theme(axis.text.x= element_text(angle=90, size=9)) +
  geom_text(aes(x=Interviewtag, y=n+100, label=n)) + 
  labs(x="Tag des Interviews", 
       y="N", 
       title="Feldzeit 3. Welle Erwerbspersonenbefragung")
