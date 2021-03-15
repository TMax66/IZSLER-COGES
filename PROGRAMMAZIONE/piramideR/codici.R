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
library("DT")
library("lubridate")
library("fmsb")

pubblicazioni <- read_excel(here("programmazione", "data", "raw", "pubblicazioni2019.xlsx"))
pubblicazioni$autore <- str_to_lower(pubblicazioni$autore)
pubblicazioni$autore <- gsub(",.*$", "", pubblicazioni$autore)


matricole <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))
matricole <- matricole %>% 
  filter(DECOMP != "COMPARTO SSN") %>% 
  select(matricola = "CDMATR", cognome = COGNOME, nome = NOME, reparto) %>% 
  mutate(cognome = str_to_lower(cognome), 
         nome = str_to_lower(nome))

matricole$autore <- str_c(matricole$cognome, matricole$nome, sep=", ")
matricole$autore <- gsub(",.*$", "", matricole$autore)

repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script


pubblicazioni %>% 
  right_join(matricole, by = "autore") %>%  
  filter(!is.na(nr)) %>% 
  select(nr, reparto, autore, tipologia, matricola, autori, titinglese, datibiblio,`TITOLO RIVISTA`, convegno, titoriginale, impf ) %>% 
  right_join(repMat, by = "matricola") %>% 
  filter(!is.na(nr)) %>% 
  saveRDS(here("programmazione", "shinyapp", "ricerca.rds"))

ricerca <- readRDS(here("programmazione", "shinyapp-in-produzione",  "ricerca.rds"))
ricerca <- ricerca %>% 
  mutate(IF = ifelse(tipologia == "IF ; Int" | tipologia == "IF",  "IF", NA), 
         INT = ifelse(tipologia == "IF ; Int" | tipologia == "Int",  "Int", NA ), 
         NAZ = ifelse(tipologia == "Naz", "Naz", NA), 
         Oth = ifelse(tipologia == "Others" , "Others", NA))

 

ricerca %>%
  filter(IF == IF) %>%
  filter (! duplicated(nr)) %>% 
  group_by(Dipartimento) %>% 
  summarise(IF= mean(impf, na.rm = TRUE),
            minIF = min(impf, na.rm = TRUE), 
            maxIF = max(impf, na.rm = TRUE), 
            MIF = median(impf, na.rm = TRUE),
            sumIF = sum(impf, na.rm = TRUE))



pr <- read_excel(here("programmazione", "data", "raw", "DatiProgettiUO.xlsx"))
repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script


repMat %>% 
  select(- Laboratorio) %>% View()

###calcola per dipartimento/reparto/tipologia e codiceprg il numero di u.o. partecipanti e il budget

pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
  left_join(repMat, by = c("MatrRSUO" = "matricola")) %>%
  


    
pr %>% 
  filter(Statoanno == "Aperto") %>% 
       group_by(Dipartimento) %>% 
  summarise(Bdg = sum(Budget), 
            MBdg = mean(Budget, na.rm = T),
            MdBdg = median(Budget, na.rm = T), 
            mdBdg = min(Budget, na.rm = T), 
            mxBdg = max(Budget, na.rm = T), 
            "Progetti di Ricerca"=nlevels(factor(Codice)))%>% 
       filter(!is.na(Dipartimento))
