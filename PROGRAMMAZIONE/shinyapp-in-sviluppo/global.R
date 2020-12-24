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

# dati <- readRDS( here("programmazione", "shinyapp", "dati.rds"))
# dati <- dati %>% 
#   mutate(across(where(is.numeric), function(x) round(x, 2)))
# vp <- readRDS( here("programmazione", "shinyapp", "vp.rds"))
# ai <- readRDS( here("programmazione", "shinyapp", "ai.rds"))

dati <- readRDS("dati.rds")
dati <- dati %>% 
  mutate(across(where(is.numeric), function(x) round(x, 2)))
vp <- readRDS("vp.rds")
ai <- readRDS("ai.rds")


###INDICATORI DI PERFORMANCES#######
dir <- dati %>%
  filter(contratto == "DIRIGENZA") %>%
  group_by(Dipartimento, Reparto) %>%
  summarise(esami = sum(esami),
            ricavi = sum(ricavi),
            FTE_d = round(sum(`FTE-reale`),1))

comp <- dati %>%
  filter(contratto == "COMPARTO") %>%
  group_by(Dipartimento, Reparto) %>%
  summarise(esami = sum(esami),
            ricavi = sum(ricavi),
            FTE_c = round(sum(`FTE-reale`),1))
vp <- vp %>% 
  group_by(Dipartimento, Reparto) %>% 
  summarise(VP = sum(`Vendita Prodotti`))

ai <- ai %>% 
  group_by(Dipartimento, Reparto) %>% 
  summarise(AI = sum(`Attività Interna`))

tabella <- dir %>%
  left_join(comp, by = c("Dipartimento", "Reparto", "esami", "ricavi")) %>% 
  left_join(vp, by = c("Dipartimento", "Reparto")) %>% 
  left_join(ai, by = c("Dipartimento", "Reparto"))



###IZSLER######_________________________________________________________________
tizsler <- tabella %>% 
  group_by(Dipartimento) %>% 
  summarise_at(c("esami", "ricavi", "FTE_d", "FTE_c", "VP", "AI"), sum) %>% 
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row", name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>%
  select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")


Tizsler <- tizsler %>%
  filter(Dipartimento != "Totale") %>% 
  left_join(
    (ricerca %>%
       filter(IF == IF) %>%
       count(Dipartimento, nr) %>%
       group_by(Dipartimento) %>%
       count(nr) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Dipartimento") %>% 
  left_join(
    (pr %>% 
       group_by(Dipartimento) %>% 
       summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
       filter(!is.na(Dipartimento))),  by = "Dipartimento")

####DSA####_____________________________________________________________________
tdsa <- tabella %>% 
  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
  group_by(Reparto) %>% 
  summarise_at(c("esami", "ricavi", "FTE_d", "FTE_c", "VP", "AI"), sum) %>% 
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row", name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>% 
  select(Reparto, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")


Tdsa <- tdsa %>%
  filter(Reparto != "Totale") %>% 
  left_join(
    (ricerca %>%
       filter(IF == IF) %>%
       count(Reparto, nr) %>%
       group_by(Reparto) %>%
       count(nr) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
  left_join(
    (pr %>% 
       group_by(Reparto) %>% 
       summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
       filter(!is.na(Reparto))),  by = "Reparto")



####DTSA#####___________________________________________________________________
tdtsa <- tabella %>% 
  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
  group_by(Reparto) %>% 
  summarise_at(c("esami", "ricavi", "FTE_d", "FTE_c", "VP", "AI"), sum) %>% 
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row", name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>% 
  select(Reparto, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")


Tdtsa <- tdtsa %>%
  filter(Reparto != "Totale") %>% 
  left_join(
    (ricerca %>%
       filter(IF == IF) %>%
       count(Reparto, nr) %>%
       group_by(Reparto) %>%
       count(nr) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
  left_join(
    (pr %>% 
       group_by(Reparto) %>% 
       summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
       filter(!is.na(Reparto))),  by = "Reparto")




####ATLOMB####__________________________________________________________________

tatlomb <- tabella %>% 
  filter(Dipartimento == "Area Territoriale Lombardia") %>% 
  group_by(Reparto) %>% 
  summarise_at(c("esami", "ricavi", "FTE_d", "FTE_c", "VP", "AI"), sum) %>% 
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row",name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>% 
  select(Reparto, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")


Tatlomb <- tatlomb %>%
  filter(Reparto != "Totale") %>% 
  left_join(
    (ricerca %>%
       filter(IF == IF) %>%
       count(Reparto, nr) %>%
       group_by(Reparto) %>%
       count(nr) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
  left_join(
    (pr %>% 
       group_by(Reparto) %>% 
       summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
       filter(!is.na(Reparto))),  by = "Reparto")


####ATER________________________________________________________________________

tater <- tabella %>% 
  filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
  group_by(Reparto) %>% 
  summarise_at(c("esami", "ricavi", "FTE_d", "FTE_c", "VP", "AI"), sum) %>% 
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row", name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>% 
  select(Reparto, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")

Tater <- tater %>%
  filter(Reparto != "Totale") %>% 
  left_join(
    (ricerca %>%
       filter(IF == IF) %>%
       count(Reparto, nr) %>%
       group_by(Reparto) %>%
       count(nr) %>%
       summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
  left_join(
    (pr %>% 
       group_by(Reparto) %>% 
       summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
       filter(!is.na(Reparto))),  by = "Reparto")

 
#########################PUBBLICAZIONI###############################################################################

# ricerca <- readRDS(here("programmazione", "shinyapp", "ricerca.rds"))

ricerca <- readRDS("ricerca.rds")

ricerca <- ricerca %>% 
  mutate(IF = ifelse(tipologia == "IF ; Int" | tipologia == "IF",  "IF", NA), 
         INT = ifelse(tipologia == "IF ; Int" | tipologia == "Int",  "Int", NA ), 
         NAZ = ifelse(tipologia == "Naz", "Naz", NA), 
         Oth = ifelse(tipologia == "Others" , "Others", NA))

#######################PROGETTI DI RICERCA#####################################

pr <- readRDS("prj.rds")

