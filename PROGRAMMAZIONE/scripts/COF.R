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


###calcola per dipartimento/reparto/tipologia e codiceprg il numero di u.o. partecipanti e il budget
x <- pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
left_join(repMat, by = c("MatrRSUO" = "matricola")) %>%
group_by(Dipartimento, Reparto, Tipologia, Codice, CodIDIzler) %>% 
  summarise(NUO= n(), 
            BudgetA = sum(Budget), 
            nproj = nlevels(factor(Codice)))%>%
  group_by(Dipartimento) %>% 
  summarise(BDG = sum(BudgetA), 
            n = sum(nproj)) %>% View()
            
#######


x %>% group_by(Tipologia) %>% 
  summarise(n = nlevels(factor(Codice)), 
            Bdg = sum(Budget)) %>% 
  janitor::adorn_totals(where = "row") #<<---- cosi calcola il numero di progetti per tipologia di tutto l'istituto



pr %>% select(-14, -15) %>% 
    mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
    filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
    mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
    left_join(repMat, by = c("MatrRSUO" = "matricola")) %>%
    group_by(Dipartimento, Reparto, Tipologia, CodIDIzler) %>% View()



!duplicated(x$Codice)

 
 