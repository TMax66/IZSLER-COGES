library(DBI)
library(odbc)
library(tidyverse)
library(dbplyr)
library(writexl)



con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)
DBI::dbListTables(con)### elenca tutte le tabelle del database
con %>% tbl("nome tabella") ## estrae il contentuo di una tabella del db

query <- "SELECT
  dbo.Personale_V2020.Matricola,
  dbo.Personale_V2020.Anno,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.Personale_V2020.Percentuale,
  dbo.Personale_V2020.Ore,
  dbo.Personale_V2020.InizioRapporto,
  dbo.Personale_V2020.FineRapporto,
  dbo.Personale_V2020.Dirigente,
  dbo.Personale_V2020.Contratto,
  dbo.Personale_V2020.Tempo,
  dbo.Personale_V2020.Nome,
  dbo.Personale_V2020.Cognome
FROM
  dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
  
WHERE
  dbo.Personale_V2020.Anno  =  2020
"


ore20 <- con %>% tbl(sql(query)) %>% as_tibble()
View(ore20)


## tabella conversione strutture
# replabdip <- read.csv("C:/Users/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/replabdip.txt", sep="")
# 
# write_xlsx(replabdip, path = "tabella.xlsx")




query2 <- "SELECT
  dbo.IZS_ANNI.ANNO,
  dbo.IZS_Reparti.CODICE_REPARTO,
  dbo.IZS_Reparti.REPARTO
FROM
  dbo.IZS_Reparti INNER JOIN dbo.IZS_CDC ON (dbo.IZS_Reparti.CODICE_REPARTO=dbo.IZS_CDC.CODICE_REPARTO)
   INNER JOIN dbo.CDC_MOVIMENTI_BO ON (dbo.IZS_CDC.CODICE_CDC=dbo.CDC_MOVIMENTI_BO.CDC)
   INNER JOIN dbo.IZS_ANNI ON (dbo.IZS_ANNI.ANNO=dbo.CDC_MOVIMENTI_BO.ANNO)
"

coge20 <- con %>% tbl(sql(query2)) %>% as_tibble()

View(coge20)


con %>% tbl(sql(query2))

query2
