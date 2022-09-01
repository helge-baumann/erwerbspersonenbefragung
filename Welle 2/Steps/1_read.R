dat1 <- read_sav("./Daten/316402781_Boeckler_Corona_Welle_1_2020_Kunde_final.sav")
dat2 <- read_dta("./Daten/316402781_Boeckler_Corona_Welle_2_2020_Kunde.dta")

names(dat1) <- paste(names(dat1), "w1", sep="_")
names(dat2) <- paste(names(dat2), "w2", sep="_")

dat <- merge(dat1, dat2, by.x = "lfdn_w1", by.y = "lfdn_W1_w2", all.x=T)

write_sav(dat, "./Daten/Erwerbspersonenbefragung_W1W2.sav")
write_dta(dat, "./Daten/Erwerbspersonenbefragung_W1W2.dta")

spss <- sapply(
  dat, function(x) { 
    if(!is.null(attributes(x)$format.spss)) return(attributes(x)$format.spss) else return(NA)

})
stata <- sapply(
  dat, function(x) { 
    if(!is.null(attributes(x)$format.stata)) return(attributes(x)$format.stata) else return(NA)
    
  })
  
type <- data.frame(name=names(dat), 
                   spss=spss, stata=stata)

write.csv2(type, "types.csv")

# long format----
panel <- read.csv2("./Daten/Panel.csv")
dat1 <- read_sav("./Daten/316402781_Boeckler_Corona_Welle_1_2020_Kunde_final.sav")
dat2 <- read_dta("./Daten/316402781_Boeckler_Corona_Welle_2_2020_Kunde.dta")

for(b in names(dat1)) {

  p1 <- panel[,1][panel[,3] == "beide" & panel[,4] == 1]
  if(b %in% p1) names(dat1[b]) <- paste(names(dat1[b]), "01", sep="_")
  if(!(b %in% p1)) names(dat1[b]) <- paste(names(dat1[b]), "w1", sep="_")

}

for(b in names(dat2)) {
  
  p2 <- panel[,2][panel[,3] == "beide" & panel[,4] == 1]
  if(b %in% p2) names(dat2[b]) <- paste(panel[panel[,2]== b,1], "02", sep="_")
  if(!(b %in% p2)) names(dat2[b]) <- paste(names(dat2[b]), "w2", sep="_")
  
}

names(dat2) <- paste(names(dat2), "w2", sep="_")

dat <- merge(dat1, dat2, by.x = "lfdn_w1", by.y = "lfdn_W1_w2", all.x=T)

write_sav(dat, "./Daten/Erwerbspersonenbefragung_W1W2.sav")
write_dta(dat, "./Daten/Erwerbspersonenbefragung_W1W2.dta")
