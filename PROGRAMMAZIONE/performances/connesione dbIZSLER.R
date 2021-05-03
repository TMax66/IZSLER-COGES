library("DBI")
library("odbc")

con <- DBI::dbConnect(odbc::odbc(),
                      
                      Driver   = "SQL Server",
                      
                      Server   =  "dbprod02.izsler.it",
                      
                      Database = "IZSLER",
                      
                      Port = 1433)


library("tidyverse")
library("dbplyr")

x <- tbl(con, "Anag_Comuni")

show_query(x)
