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

tizsler %>% 
  left_join(
    (ricerca %>% 
       filter(IF == IF) %>%  
       count(Dipartimento, nr) %>% 
       group_by(Dipartimento) %>% 
       count(nr) %>% 
       summarise("Pubblicazioni" = sum(n)) %>% 
       bind_rows(data.frame("Pubblicazioni" =(ricerca %>% 
                                                filter(IF == "IF") %>% 
                                                group_by(nr) %>% 
                                                count(nr) %>% 
                                                select(nr) %>% 
                                                nrow()))) %>% 
       replace_na(list(Dipartimento ="Totale"))), by = "Dipartimento") %>% 
  left_join(
    (pr %>% 
       
      
      
      
    )
    
    
    
  )




 























###calcola per dipartimento/reparto/tipologia e codiceprg il numero di u.o. partecipanti e il budget

pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
  left_join(repMat, by = c("MatrRSUO" = "matricola")) %>%
  saveRDS(here("programmazione", "shinyapp", "prj.rds"))
  
  
  pr %>% 
  group_by(Tipologia) %>% 
  summarise(n = nlevels(factor(Codice)), 
            Bdg = sum(Budget)) %>% 
  janitor::adorn_totals(where = "row") #<<---- cosi calcola il numero di progetti per tipologia di tutto l'istituto

  
  pr %>% 
    summarise(n = nlevels(factor(Codice)))
  
  
  pr %>% 
    group_by(Dipartimento) %>% 
    summarise(n=nlevels(factor(Codice))) %>% 
    filter(!is.na(Dipartimento))
  
  

####elenco PR di IZSLER
pr %>%
  group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
  summarise(Budget = sum(Budget), nUO = n()) %>%
  

  pr$DataFine-pr$DataInizio

as.Date("2019/12/31")-(pr$DataInizio)


x <- pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
left_join(repMat, by = c("MatrRSUO" = "matricola")) %>%
  
  pr %>% 
group_by(Dipartimento, Reparto, Tipologia, Codice, CodIDIzler) %>% 
  summarise(NUO= n(), 
            BudgetA = sum(Budget), 
            nproj = nlevels(factor(Codice)))%>%
  group_by(Dipartimento) %>% 
  summarise(BDG = sum(BudgetA), 
            n = sum(nproj)) %>% View()
            
#######

pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
  left_join(repMat, by = c("MatrRSUO" = "matricola")) %>%
  group_by(Tipologia) %>% 
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

#############################
 