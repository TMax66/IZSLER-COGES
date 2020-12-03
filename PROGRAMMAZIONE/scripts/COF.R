library("tidyverse")
library("readxl")
library("RColorBrewer")
library("shiny")
library("shinydashboard")
library("here")
library("janitor")
library("here")
library("flextable")
library("shinyBS")
library("officer")

pr <- read_excel(here("programmazione", "data", "raw", "DatiProgettiUO.xlsx"))
repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script



pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Aperto")) %>% 
  filter(Stato == "Aperto" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Attivo")) %>% View()
  



left_join(repMat, by = c("MatrRSUO" = "matricola")) %>% View()

  
    
group_by(Dipartimento, Reparto, NumUoProgetto, Codice) %>% View()

 





 