library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")
library("stringr")

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

repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds"))


# pubblicazioni %>% 
#   right_join(matricole, by = "autore") %>%  
#   filter(!is.na(nr)) %>% 
#   select(nr, reparto, autore, tipologia, matricola, autori, biblio) %>% 
#   right_join(repMat, by = "matricola") %>% 
#   filter(!is.na(nr)) %>% 
#   # mutate(id = seq(1:717)) %>% 
#   # pivot_wider(names_from = autore, values_from = autore)
#   group_by(Dipartimento, tipologia) %>% 
#   count(nr) %>%  
#   summarise(n.articoli = n()) %>% 
#   pivot_wider(names_from = tipologia, values_from = n.articoli)
#   #non si può fare la somma per colonna  perchè gli articoli sono comuni a diversi reparti####




pubblicazioni %>% 
  right_join(matricole, by = "autore") %>%  
  filter(!is.na(nr)) %>% 
  select(nr, reparto, autore, tipologia, matricola, autori, titinglese, datibiblio,`TITOLO RIVISTA`, convegno, titoriginale, impf ) %>% 
  right_join(repMat, by = "matricola") %>% 
  filter(!is.na(nr)) %>% 
  saveRDS(here("programmazione", "shinyapp", "ricerca.rds"))
  
  
  
  
 
  
  
  
 


