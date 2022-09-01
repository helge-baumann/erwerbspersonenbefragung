wb <- createWorkbook()

for(b in names(Ergebnis_quer)) {
  
  addWorksheet(wb, b) 
  
  nonselbst <- which(Ergebnis_quer[[b]]$table[,2] == "Arbeiter/in, Angestellte/r, Beamte/r")
  selbst <- which(Ergebnis_quer[[b]]$table[,2] == "Selbständig")
  
  #createStyle
  writeData(wb, b, Ergebnis_quer[[b]]$label, startCol=1, startRow=1)
  writeDataTable(wb, b, Ergebnis_quer[[b]]$table, startCol=1, startRow =3)
  
}

saveWorkbook(wb, file=paste0("./Output/", Sys.Date(), "_Selbstaendige_quer.xlsx"), overwrite=T)

wb <- createWorkbook()

for(b in names(Ergebnis_laengs)) {
  
  addWorksheet(wb, b) 
  
  nonselbst <- which(Ergebnis_laengs[[b]]$table[,2] == "Arbeiter/in, Angestellte/r, Beamte/r")
  selbst <- which(Ergebnis_laengs[[b]]$table[,2] == "Selbständig")
  
  #createStyle
  writeData(wb, b, Ergebnis_laengs[[b]]$label, startCol=1, startRow=1)
  writeDataTable(wb, b, Ergebnis_laengs[[b]]$table, startCol=1, startRow =3)
  
}

saveWorkbook(wb, file=paste0("./Output/", Sys.Date(), "_Selbstaendige_laengs.xlsx"), overwrite=T)
