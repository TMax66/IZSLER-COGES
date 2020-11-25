library("tidyverse")
library("networkD3")
library("hrbrthemes")
library("readxl")
library("ggrepel")
library("RColorBrewer")
library("wesanderson")
library("DT")
library("shiny")
library("shinydashboard")
library("here")
library("knitr")
library("kableExtra")
library("formattable")
library("shinythemes")
library("rpivotTable")
library("janitor")
library("here")
library("flextable")
options(scipen = .999)
dati <- readRDS( here("programmazione", "shinyapp", "dati.rds"))
dati <- dati %>% 
  mutate(across(where(is.numeric), function(x) round(x, 2)))
vp <- readRDS( here("programmazione", "shinyapp", "vp.rds"))
ai <- readRDS( here("programmazione", "shinyapp", "ai.rds"))

dir <- dati %>%
  filter(contratto == "DIRIGENZA") %>%
  group_by(Dipartimento) %>%
  summarise(esami = sum(esami),
            ricavi = sum(ricavi),
            FTE_d = round(sum(`FTE-reale`),1))

comp <- dati %>%
  filter(contratto == "COMPARTO") %>%
  group_by(Dipartimento) %>%
  summarise(esami = sum(esami),
            ricavi = sum(ricavi),
            FTE_c = round(sum(`FTE-reale`),1))
vp <- vp %>% 
  group_by(Dipartimento) %>% 
  summarise(VP = sum(`Vendita Prodotti`))

ai <- ai %>% 
  group_by(Dipartimento) %>% 
  summarise(AI = sum(`Attività Interna`))

tabella <- dir %>%
  bind_cols((comp %>%
               select(4)), (vp %>%
                              select(2)), (ai %>%
                                             select(2))) %>%
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>%
  select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")

#####################################################################################################################
#########################PUBBLICAZIONI###############################################################################
#####################################################################################################################

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


ricerca <- pubblicazioni %>% 
  right_join(matricole, by = "autore") %>%  
  filter(!is.na(nr)) %>% 
  select(nr, reparto, autore, tipologia, matricola, autori, titinglese, datibiblio) %>% 
  right_join(repMat, by = "matricola") %>% 
  filter(!is.na(nr)) # %>% 
  # mutate(id = seq(1:717)) %>% 
  # pivot_wider(names_from = autore, values_from = autore)
  # group_by(Dipartimento, tipologia) %>% 
  # count(nr) %>%  
  # summarise(n.articoli = n()) %>% 
  # pivot_wider(names_from = tipologia, values_from = n.articoli)
#non si può fare la somma per colonna  perchè gli articoli sono comuni a diversi reparti####



