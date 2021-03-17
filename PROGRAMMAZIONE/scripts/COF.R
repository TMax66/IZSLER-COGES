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


dtProg <- readRDS( here("programmazione", "shinyapp-in-sviluppo", "datiSB.rds"))
dtProg %>% 
  group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
  group_by(Reparto) %>% 
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>% 
  group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>% 
  summarise(FTEDp = sum(FTEDp)) %>%
  pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%  
  arrange(desc(`Obiettivi Valorizzati`)) 


dtProg %>% 
  group_by(obcod, Obiettivo, Valorizzazione, Dipartimento) %>% 
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  group_by( Dipartimento) %>% 
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>% 
  group_by(Dipartimento, "Obiettivi Valorizzati" = Valorizzazione) %>% 
  summarise(FTEDp = sum(FTEDp)) %>%
  pivot_wider(names_from = "Dipartimento", values_from = "FTEDp") %>%  
  arrange(desc(`Obiettivi Valorizzati`)) 



dtProg %>% 
  group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
  group_by(Reparto) %>%
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>%  
  pivot_wider(id_cols = 1:5, 
              names_from = "Reparto", values_from = "FTECp") %>% 
  mutate(total = rowSums(across(where(is.numeric))))%>% 
  filter(total > 0.00000000) %>% 
  arrange(desc(Valorizzazione)) %>% 
  select(-total) %>% 
  column_to_rownames(var = "obcod") %>% View()





dtProg %>% 
  group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>%
  group_by(Reparto) %>%
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>%  
  pivot_wider(id_cols = 1:5, 
              names_from = "Reparto", values_from = "FTEDp") %>% 
  mutate(total = rowSums(across(where(is.numeric))))%>% 
  filter(total > 0.00000000) %>% 
  arrange(desc(Valorizzazione)) %>% 
  select(-total) %>% 
  column_to_rownames(var = "obcod")



dtProg %>% 
  group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% View()
  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>%
  group_by(Reparto) %>%
  mutate(FTEDp = prop.table(FTED), 
         FTECp = prop.table(FTEC) ) %>%  
  pivot_wider(id_cols = 1:5, 
              names_from = "Reparto", values_from = "FTECp") %>% 
  mutate(total = rowSums(across(where(is.numeric))))%>% 
  filter(total > 0.00000000) %>% 
  arrange(desc(Valorizzazione)) %>% 
  select(-total, -Dipartimento) %>% 
  column_to_rownames(var = "obcod")  %>% View()
