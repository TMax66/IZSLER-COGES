library("tidyverse")
library("readxl")
#library("RColorBrewer")
# library("shiny")
# library("shinydashboard")
library("here")
library("janitor")
library("flextable")
library("ztable")
# library("shinyBS")
# library("officer")
# library("fmsb")
library("knitr")

dt <- readRDS( here("programmazione", "shinyapp-in-produzione", "datiSB.rds"))
  
dt %>% 
  group_by( Dipartimento) %>% 
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>% 
  group_by(Dipartimento, Valorizzazione) %>% 
  summarise(FTEDp = sum(FTEDp)) %>%
  pivot_wider(names_from = "Dipartimento", values_from = "FTEDp") %>% 
    mutate(total = rowSums(across(where(is.numeric))))%>% 
    arrange(desc(Valorizzazione)) %>% 
    select(-total, ) %>% 
    View() 
    
x <- dt%>% 
  group_by( Dipartimento) %>% 
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>% 
  group_by(Dipartimento, "Obiettivi Valorizzati" = Valorizzazione) %>%
  summarise(FTEDp = sum(FTEDp)) %>%
  pivot_wider(names_from = "Dipartimento", values_from = "FTEDp") %>%  
  arrange(desc(`Obiettivi Valorizzati`)) 
