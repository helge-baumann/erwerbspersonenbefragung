wb <- createWorkbook()

headerStyle <- createStyle(fontSize = 12, fontColour = "red", halign = "left",
                           textDecoration="bold")
genStyle <-  createStyle(fontSize = 10, fontColour = "blue", halign = "left",
                         textDecoration="italic")

posStyle <- createStyle(fontColour = "blue", halign = "right")
negStyle <- createStyle(fontSize = 10, fontColour = "red", halign = "right")

posBorder <- createStyle(borderColour = "blue", border="TopBottomLeftRight", borderStyle="double")
negBorder <- createStyle(borderColour = "red", border="TopBottomLeftRight", borderStyle="double")

# Inhaltsverzeichnis
addWorksheet(wb, "Inhalt")

writeData(wb, "Inhalt", "Inhaltsverzeichnis", startCol=1, startRow=1)

num <- 0

for(b in names(Ergebnis)) {
  
  name <- str_remove(b, fixed("_w2"))
  
  addWorksheet(wb, name)
  
  # Name 2
  writeData(wb, name, "Variablenname Welle 2:", startCol=1, startRow=1)
  writeData(wb, name, name, startCol=2, startRow=1)
  addStyle(wb, name, style=posStyle, cols=2, rows=1)
  
  writeData(wb, name, "Variablenlabel Welle 2:", startCol=1, startRow=2)
  writeData(wb, name, meta$l2[(meta$n2 == name) %in% T], startCol=2, startRow=2)
  addStyle(wb, name, style=posStyle, cols=2, rows=2)
  
  # Name 1
  writeData(wb, name, "Variablenname Welle 1:", startCol=1, startRow=3)
  writeData(wb, name, meta$n1[(meta$n2 == name) %in% T], startCol=2, startRow=3)
  addStyle(wb, name, style=posStyle, cols=2, rows=3)
  
  writeData(wb, name, "Variablenlabel Welle 1:", startCol=1, startRow=4)
  writeData(wb, name, meta$l1[(meta$n2 == name) %in% T], startCol=2, startRow=4)
  addStyle(wb, name, style=posStyle, cols=2, rows=4)
  
  # identisch?
  writeData(wb, name, "Welle 1 + 2 identisch:", startCol=1, startRow=5)
  if(meta$identisch[(meta$n2 == name) %in% T] == 1) writeData(wb, name, "ja", startCol=2, startRow=5)
  if(meta$identisch[(meta$n2 == name) %in% T] == 0) writeData(wb, name, "nein", startCol=2, startRow=5)
  addStyle(wb, name, style=posStyle, cols=2, rows=5)
  
  # Hinweis
  writeData(wb, name, "Hinweis:", startCol=1, startRow=6)
  writeData(wb, name, meta$Anmerkung[(meta$n2 == name) %in% T], startCol=2, startRow=6)
  addStyle(wb, name, style=posStyle, cols=2, rows=6)
  
  writeData(wb, name, "Ergebnisse:", startCol=1, startRow=8)
  
  # Tabelle
  
  # Namen der Ergebnisse
  Ergebnis[[b]] <- Ergebnis[[b]][,
    !str_detect(tolower(names(Ergebnis[[b]])), "<na>|weiß nicht|keine angabe")]
  
  w1 <- "N.y" %in% names(Ergebnis[[b]])
  if(w1) {
    
    pos <- which(names(Ergebnis[[b]]) == "N.y")
    
    writeData(wb, name, "Welle 2:", startRow=9, startCol=3)
    writeData(wb, name, "Welle 1:", startRow=9, startCol=pos)
    mergeCells(wb, name, cols=3:(pos-1), rows=9)
    mergeCells(wb, name, cols=pos:ncol(Ergebnis[[b]]), rows=9)
    
    names(Ergebnis[[b]]) <- str_remove(names(Ergebnis[[b]]), fixed(".x"))
    names(Ergebnis[[b]]) <- str_remove(names(Ergebnis[[b]]), fixed(".y"))
    names(Ergebnis[[b]])[3:(pos-1)] <- 
      paste0(names(Ergebnis[[b]])[3:(pos-1)], " Welle 2")
    names(Ergebnis[[b]])[pos:ncol(Ergebnis[[b]])] <- 
      paste0(names(Ergebnis[[b]])[pos:ncol(Ergebnis[[b]])], " Welle 1")
    
  }
  
  # Duplikate im Namen?
  names(Ergebnis[[b]])[duplicated(tolower(names(Ergebnis[[b]])))] <-
  paste0(names(Ergebnis[[b]])[duplicated(tolower(names(Ergebnis[[b]])))], ".y") 
  
  writeDataTable(wb, name, Ergebnis[[b]], startCol=1, startRow = 10,
                 firstColumn = T, tableStyle="TableStyleMedium2")
  #addStyle(wb, b, negStyle, rows=26:45, cols=4)
  
  #addStyle(wb, b, headerStyle, rows=1, cols=1)
  #addStyle(wb, b, genStyle, rows=c(3), cols=1:100)
  
  # set style for cells
  check <- Ergebnis[[b]]
  check[,str_detect(names(check), "Variable|Ausprägung|^N|Ungültig|^Gültig|^miss")] <- NA
  
  for(i in 2:nrow(check)) {
    for(j in 3:ncol(check)) {
      
      if(!any(str_detect(names(Ergebnis[[b]]), "Welle 1"))) {
      
      abstand <- (check[i,j]-check[1,j])/check[i,j]
      
      if(abstand <= -0.2 & !is.na(abstand)) addStyle(wb, name, negBorder, rows=i+10, cols=j)
      if(abstand >= 0.2 & !is.na(abstand)) addStyle(wb, name, posBorder, rows=i+10, cols=j)
      
      } else {
        
         pos <- which(names(Ergebnis[[b]]) == "N Welle 1")
         
         if(j < pos) {
           
        abstand <- (check[i,j]-check[1,j])/check[i,j] 
        
         if(abstand <= -0.2 & !is.na(abstand)) addStyle(wb, name, negBorder, rows=i+10, cols=j)
         if(abstand >= 0.2 & !is.na(abstand)) addStyle(wb, name, posBorder, rows=i+10, cols=j)
         
         } else {
           
           colname1 <- str_remove(names(Ergebnis[[b]])[j], " Welle 1")
           col2 <- which(str_remove(names(Ergebnis[[b]]), " Welle 2") == colname1)
           
           if(length(col2) > 0) {
             
           abstand <- (check[i,j]-check[i,col2])/check[i,j] 
           
           if(abstand <= -0.2 & !is.na(abstand)) addStyle(wb, name, negBorder, rows=i+10, cols=j)
           if(abstand >= 0.2 & !is.na(abstand)) addStyle(wb, name, posBorder, rows=i+10, cols=j)
           
           }
           
         }
      }
    }
  }
  
  setColWidths(wb, name, cols=1, widths = 16)
  setColWidths(wb, name, cols=2, widths = 32)
  
  print(b)
  
  num <- num+1
  
  if(num %in% seq(from=1, to=ncol(dat)-20, by=20)) {
    
    num <- num+1
    
    writeData(wb, "Inhalt", 
              paste0(name, " - ", vars[which(vars == b)+17]), startCol=1, startRow=num+2)
    
    num <- num+1
    
  }
  
  
  writeFormula(
    wb, "Inhalt", 
    makeHyperlinkString(name, row = 1, col = 1, text = name, file = NULL),
    startCol = 1, startRow = num+2
  )
  writeData(
    wb, "Inhalt",
    meta$l2[(meta$n2 == name) %in% T], 
    startCol=2, startRow=num+2
  )
  
  
}



saveWorkbook(wb, "./Output/Output.xlsx", overwrite=T)

names_labels <- data.frame(name=NA, label=NA)
num <- 0

for(b in names(Ergebnis)) {
  
  #name <- str_remove(b, fixed("_w2"))
  num <- num+1
  names_labels[num,1] <- b
  if(!is.null(attributes(dat[[b]])$label)) {
    names_labels[num,2] <- attributes(dat[[b]])$label
  }
  
}

write.csv2(names_labels, "./Output/namen.csv")


