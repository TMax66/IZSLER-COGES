library("DBI")
library("odbc")
library("tidyverse")

##connessione al db####
con <- DBI::dbConnect(odbc::odbc(),
                      
                      Driver   = "SQL Server",
                      
                      Server   =  "CED-IIS2.izsler.it",
                      
                      Database = "ObiettiviStrategiciV2018",
                      
                      Port = 1433)


# query <- c("SELECT Descrizione FROM dbo.ObiettivoStrategico")
# 
# os <- tbl(con, sql("SELECT Prospettiva_ProspettivaId, Descrizione FROM dbo.ObiettivoStrategico"))
# prosp <- tbl(con, sql("SELECT * FROM dbo.Prospettiva"))
# os %>% 
#    left_join(prosp, by= c("Prospettiva_ProspettivaId" = "ProspettivaId")) %>% data.frame() %>%  View()
# openT <- function( db, tabella, ...)
#           {


####codice per bypassare bug di odbc####
column.types <- dbGetQuery(con, "SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH
                           FROM INFORMATION_SCHEMA.COLUMNS 
                           WHERE TABLE_NAME=  'SchedaValutazione' " )
ct <- column.types %>%
  mutate(cml = case_when(
    is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
    CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
    TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
  )
  ) %>%
  arrange(cml) %>%
  pull(COLUMN_NAME)
fields <- paste(ct, collapse=", ")
query <- paste("SELECT", fields, paste("FROM", "SchedaValutazione"))

# return(query)
# }

#query <- openT(db = con, tabella = Audit)

#estraggo la tabella SchedaValutazione##
SV <- tbl(con, sql(query)) %>% 
  as_tibble()
names(SV)

SV
