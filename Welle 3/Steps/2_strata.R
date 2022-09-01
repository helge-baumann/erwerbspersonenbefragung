# Schichtungsvariablen

# Größe
dat <- dat %>% 
  mutate(Alter= cut(S1__1, breaks=c(0, 30, 45, 60, 100),
                    labels=c("(1) bis 30", "(2) 31 bis 45", "(3) 46 bis 60", "(4) 61 und älter")
                    ), 
         Geschlecht = as_factor(S2__1),
         Kinder = if_else(S11_3__1 > 1 & S11_3__1 < 99, 1, 0),
         Branche = as_factor(S5__1),
         BG = as_factor(F32__1),
         BR = as_factor(D9__3),
         Alle = "Gesamt"
  )
dat$Geschlecht <- recode_factor(dat$Geschlecht, Divers = NA_character_)
dat$BG <- recode_factor(dat$BG, "Keine Angabe" = NA_character_)
dat$BR <- recode_factor(dat$BR, "Weiß nicht" = NA_character_)
dat$Kinder <- factor(dat$Kinder, labels=c("keine Kinder unter 14", "Kinder unter 14"))
attributes(dat$Alle)$label <- "Alle Personen"
attributes(dat$Alter)$label <- "Alter"
attributes(dat$Geschlecht)$label <- "Geschlecht"
attributes(dat$Kinder)$label <- "Kinder unter 14 im Haushalt"
attributes(dat$Branche)$label <- "Branche"
attributes(dat$BG)$label <- "Betriebsgröße"




         
         