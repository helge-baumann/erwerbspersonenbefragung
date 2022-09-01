dat1 <- read_sav("./Daten/316402781_Boeckler_Corona_Welle_1_2020_Kunde_final.sav")
dat2 <- read_sav("./Daten/316402781_Boeckler_Corona_Welle_2_2020_Kunde.sav")
dat3 <- read_sav("./Daten/Boeckler_Corona_Welle3_2020_Kunde.sav")

# wenn: einzigartig, dann ein Wellenkennzeichen, ansonsten: __1, __2, __3
names(dat1) <- paste(names(dat1), "1", sep="__")
names(dat2) <- paste(names(dat2), "2", sep="__")
names(dat3) <- paste(names(dat3), "3", sep="__")

# merge----
dat <- dat1 %>% 
  full_join(dat2, by=c("lfdn__1" = "lfdn_W1__2")) %>%
  full_join(dat3, by=c("lfdn__1" = "lfdn_W1__3")) 


#write_sav(dat, "./Daten/Erwerbspersonenbefragung_W1W2.sav")
write_dta(dat, "./Daten/Erwerbspersonenbefragung_Wellen1-3_wide.dta")
write_sav(dat, "./Daten/Erwerbspersonenbefragung_Wellen1-3_wide.sav")


lab <- function(x) {
  
  lab <- attributes(x)$label
  if(!is.null(lab)) return(lab) else return(NA)
  
}

nl <- 
  data.frame(
    n3 = names(dat3),
    l3 = unname(sapply(dat3, lab))
  )

write.csv2(nl, "./Abgleich/names_labs_w3.csv")


rm(dat1)
rm(dat2)
rm(dat3)