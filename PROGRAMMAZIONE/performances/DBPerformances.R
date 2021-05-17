library(dbplyr)
library(dplyr)


con2 <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2.izsler.it", 
                       Database = "ObiettiviStrategiciV2018", Port = 1433)


src_dbi(con2)

ind <- tbl(con2, "Indicatore")

ind %>% 
  select(Descrizione) %>% data.frame() %>% View()


head(ind, n=10)


tbl(con2, "Audit")


dbColumnInfo(dbSendQuery(con2, "SELECT * FROM Audit"))
 

index <- dbColumnInfo(dbSendQuery(con2, "SELECT * from Audit"))
index$type <- as.integer(index$type) # B/c they are + and - numbers!


mySQLTbl <- tbl(con2, in_schema("schema", "Audit"))


dbGetQuery(con2, "SELECT TOP 10 * FROM dbo.Audit")
