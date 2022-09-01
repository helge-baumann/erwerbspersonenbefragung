# Speichern Längsschnitt

wb <- createWorkbook()

headerStyle <- createStyle(fontSize = 12, fontColour = "red", halign = "left",
                           textDecoration="bold")
genStyle <-  createStyle(fontSize = 10, fontColour = "blue", halign = "left",
                         textDecoration="italic")

posStyle <- createStyle(fontColour = "blue", halign = "right")
negStyle <- createStyle(fontColour = "red", halign = "right")
diagStyle <- createStyle(fontColour = gray(0.5), halign = "right")

posBorder <- createStyle(borderColour = "blue", border="TopBottomLeftRight", borderStyle="double")
negBorder <- createStyle(borderColour = "red", border="TopBottomLeftRight", borderStyle="double")

# Inhaltsverzeichnis
addWorksheet(wb, "Inhalt")

writeData(wb, "Inhalt", "Inhaltsverzeichnis", startCol=1, startRow=1)

num <- 0

for(b in names(crosstab)) {
  
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
  
  writeData(wb, name, "Welle 2:", startCol=3, startRow=9)
  writeData(wb, name, "Welle 1:", startCol=1, startRow=11)
  
  writeData(wb, name, crosstab[[b]], startCol=2, startRow=10)
  
  for(i in 1:nrow(crosstab[[b]])) {
    for(j in 1:ncol(crosstab[[b]])) {
      
      if(i == j) {
        addStyle(wb, name, diagStyle, rows=i+10, cols=j+2)
      } else {
        if(i < j) {
        if(crosstab[[b]][i,j] > crosstab[[b]][j,i]+(crosstab[[b]][j,i]*0.2)) {
          addStyle(wb, name, posStyle, rows=i+10, cols=j+2)
        }
        if(crosstab[[b]][i,j] < crosstab[[b]][j,i]-(crosstab[[b]][j,i]*0.2)) {
          addStyle(wb, name, negStyle, rows=i+10, cols=j+2)
        }
        }
      
    }
    
    }
  }
  
  
  
  #setColWidths(wb, name, cols=1, widths = 16)
  #setColWidths(wb, name, cols=2, widths = 32)
  
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

saveWorkbook(wb, "./Output/Output_Laengs.xlsx", overwrite=T)




