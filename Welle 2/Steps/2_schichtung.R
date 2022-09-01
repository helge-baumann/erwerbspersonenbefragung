# Schichtung: Alter, Geschlecht, Schulabschlkuss, Bundesland, 
# Branche, Gewerk, Mig, Einkommen

dat <- dat %>%
  mutate(
    # wellenspezifisch 
    alter1 = 
      cut(S1_w1, 
          breaks =c(0, 25, 35, 45, 55, 65, 100), 
          labels = c("unter 25", "25 bis 34", "35 bis 44", "45 bis 54",
                     "55 bis 64", "65 und älter"),
          right = T),
    alter2 = 
      cut(S1_w2, 
          breaks =c(0, 25, 35, 45, 55, 65, 100), 
          labels = c("unter 25", "25 bis 34", "35 bis 44", "45 bis 54",
                     "55 bis 64", "65 und älter"),
                     right = T),
    sex1 = as_factor(S2_w1), sex2 = as_factor(S2_w2),
    schul1 = as_factor(S3_w1), schul2 = as_factor(S3_w2),
    branche1 = as_factor(S5_w1),  branche2 = as_factor(S5_w2),
    job1 = as_factor(S6_w1), job2 = as_factor(S6_w2),
    bg1 = as_factor(F32_w1), bg2 = as_factor(D7_w2),
    tv1 = as_factor(F33_w1), tv2 = as_factor(D8_w2),
    br1 = as_factor(F34_w1), br2 = as_factor(D9_w2),
    # Angaben aus Welle 1 übertragen
    bland = as_factor(S4_w1),
    p18 = 
      cut(S11_1_w1,
          breaks = c(0, 2,3,20),
          labels=c("eine Person", "zwei Personen", "drei Personen und mehr"),
          right=T
      ),
    p1418 = 
      cut(S11_2_w1,
          breaks = c(0, 1,2,20),
          labels=c("keine Person", "eine Person", "zwei Personen und mehr"),
          right=T
      ),
    p14 = 
      cut(S11_3_w1,
          breaks = c(0, 1,2, 20),
          labels=c("keine Person", "eine Person", "zwei Personen und mehr"),
          right=T
      ),
    zimmer = 
      cut(S12o_w1, 
          breaks = c(0, 1, 3, 5, 99), 
          labels= c("ein Zimmer", "zwei oder drei Zimmer", "vier oder fünf Zimmer",
                    "sechs Zimmer und mehr"),
          right=T),
    gew = as_factor(S15_w1),
    mig = as_factor(S16_w1),
    all = "Gesamt"
         )

attributes(dat$alter1)$label <- "Alter der Zielperson"
attributes(dat$alter2)$label <- "Alter der Zielperson"
attributes(dat$p18)$label <- "Personen über 18 im HH"
attributes(dat$p1418)$label <- "Personen zwischen 14 und 18 im HH"
attributes(dat$p14)$label <- "Personen unter 14 im HH"
attributes(dat$zimmer)$label <- "Anzahl Zimmer"
attributes(dat$all)$label <- "Alle Personen"
attributes(dat$bg1)$label <- "D7 Betriebsgröße"
attributes(dat$bg2)$label <- "D7 Betriebsgröße"
attributes(dat$tv1)$label <- "D8 Tarifvertrag"
attributes(dat$tv2)$label <- "D8 Tarifvertrag"
attributes(dat$br1)$label <- "D9 Betriebsrat"
attributes(dat$br2)$label <- "D9 Betriebsrat"

strata1 <- c(
  "alter1", "sex1", "schul1", "branche1", "job1", "bg1", "tv1", "br1",
  "bland", "p18", "p1418", "p14", "zimmer", "gew", "mig", "all")
strata2 <- c(
  "alter2", "sex2", "schul2", "branche2", "job2", "bg2", "tv2", "br2",
  "bland", "p18", "p1418", "p14", "zimmer", "gew", "mig", "all")

