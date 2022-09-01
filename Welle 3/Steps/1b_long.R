# Longformat aus Abgleich (manuell statt automatisch)

abgleich <- read.xlsx(
  "./Abgleich/2020-12-17_Erwerbspersonenbefragung_Abgleich-Welle123.xlsx", 
  startRow=2
)

names(abgleich) <- c("n1", "l1", "n2", "l2", "n3", "l3", "erh", "iden", "anm")

for(i in 1:nrow(abgleich)) {
  
  for(j in 1:3) {
    
    # Um welche Variable geht es?
    var <- abgleich[i, paste0("n", j)]
    
    # welche Variable im Datensatz heißt so?
    n <- which(names(dat) == paste0(var, "__", j))
    
    # Namen hinterlegen für leichtere Programmierung
    name <- names(dat)[n]
    
    # Namen ändern
    
      # Wenn j in iden 
      if(length(n) > 0 & str_detect(abgleich$iden[i], as.character(j))) {
        
        names(dat)[n] <- 
          str_replace(name, paste0("__", j), paste0("_w", abgleich$iden[i], "__", j))
        
        # Namen anpassen an vorherige Welle
        if(j == 2 & str_detect(abgleich$iden[i], "1")) {
          
          names(dat)[n] <- str_replace(names(dat)[n], abgleich[i,paste0("n", 2)], abgleich[i, paste0("n", 1)])
          attributes(dat[[names(dat)[n]]])$label <- abgleich[i, paste0("l", 1)]
          
        }
        
        if(j == 3 & str_detect(abgleich$iden[i], "1")) {
          
          names(dat)[n] <- str_replace(names(dat)[n], abgleich[i,paste0("n", 3)], abgleich[i, paste0("n", 1)])
          attributes(dat[[names(dat)[n]]])$label <- abgleich[i, paste0("l", 1)]
          
        }
        
        if(j == 3 & str_detect(abgleich$iden[i], "2") & !str_detect(abgleich$iden[i], "1")) {
          
          names(dat)[n] <- str_replace(names(dat)[n], abgleich[i,paste0("n", 3)], abgleich[i, paste0("n", 2)])
          attributes(dat[[names(dat)[n]]])$label <- abgleich[i, paste0("l", 2)]
          
        }
        
      }
      
      # Wenn nicht: eigene Variable
      if(length(n) > 0 & !str_detect(abgleich$iden[i], as.character(j))) {
        
        names(dat)[n] <- 
        str_replace(name, paste0("__", j), paste0("_w", j, "__", j))
      
      }
    
   
  
  }

  print(i)
  
}

dat_long <- dat %>% 
  pivot_longer(
    cols = -lfdn_w123__1, 
    names_to = c('.value', 'Welle'), 
    names_sep="__")

dat_long$Welle <- as.numeric(dat_long$Welle)

write_dta(dat_long, "./Daten/Erwerbspersonenbefragung_Wellen1-3_long_man.dta")
write_sav(dat_long, "./Daten/Erwerbspersonenbefragung_Wellen1-3_long_man.sav")