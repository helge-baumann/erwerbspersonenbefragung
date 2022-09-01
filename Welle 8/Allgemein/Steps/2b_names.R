# Abgleich: Wo welcher Name zu finden?

n <- nrow(abgleich)

namen <- tibble(
  w1_orig = character(n), w1_wide = character(n), w1_long = character(n),
  w2_orig = character(n), w2_wide = character(n), w2_long = character(n),
  w3_orig = character(n), w3_wide = character(n), w3_long = character(n),
  w4_orig = character(n), w4_wide = character(n), w4_long = character(n),
  w5_orig = character(n), w5_wide = character(n), w5_long = character(n),
  w6_orig = character(n), w6_wide = character(n), w6_long = character(n),
  w7_orig = character(n), w7_wide = character(n), w7_long = character(n),
  w8_orig = character(n), w8_wide = character(n), w8_long = character(n)
)

for(i in 1:nrow(abgleich)) {
  
  for(j in 1:8) {
    
    # replace identisch: nur, wenn auch erhoben!
    iden <- 
      paste(intersect(unlist(str_split(abgleich$iden[i], "")), unlist(str_split(abgleich$erh[i], ""))), 
            collapse="")
    
    # Namen ändern
    
    # Wenn j in iden (identisch) 
    if(str_detect(iden, as.character(j))) {
      
      # Namen anpassen an vorherige Welle (bei Namensänderung)
      first <- substr(abgleich$iden[i], 1, 1)
      
      namen[i, j*3-2] <- abgleich[i, j*2-1]
      namen[i, j*3-1]<- paste0(abgleich[i, as.numeric(first)*2-1], "__", j)
      namen[i, j*3]<- paste0(abgleich[i, as.numeric(first)*2-1], "_w", iden)
      
    }
    
    # Wenn j in iden (identisch) 
    if(iden == "" & !is.na(abgleich[i, j*2]) & abgleich[i, j*2] != "") {
      
      namen[i, j*3-2] <- abgleich[i, j*2-1]
      namen[i, j*3-1]<- paste0(abgleich[i, j*2-1], "__", j)
      namen[i, j*3]<- paste0(abgleich[i, j*2-1], "_w", j)
      
    }
    
    # Wenn j nicht in iden (identisch) 
    if(!str_detect(iden, as.character(j)) & !is.na(abgleich[i, j*2]) & abgleich[i, j*2] != "") {
      
      namen[i, j*3-2] <- abgleich[i, j*2-1]
      namen[i, j*3-1]<- paste0(abgleich[i, j*2-1], "__", j)
      namen[i, j*3]<- paste0(abgleich[i, j*2-1], "_w", j)
      
    }
    
  }
  
  print(i)
  
}

write.csv2(namen, "Output/namen.csv")
