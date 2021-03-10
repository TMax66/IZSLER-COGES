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
  
obfted <- read_excel(here("programmazione", "data", "raw", "obiettiviXSB.xlsx"), sheet = "FTEDdipsan")
obftec <- read_excel(here("programmazione", "data", "raw", "obiettiviXSB.xlsx"), sheet = "FTECdipsan")

dtD <- obfted %>% 
  mutate(obcod = paste("OB", seq(1:nrow(.)))) %>% 
  pivot_longer(3:27, names_to = "struttura", values_to = "FTED") %>% 
  mutate(reparto = recode(struttura, "STBO" = "STBO-FE-MO", 
                          "STFE" = "STBO-FE-MO", 
                          "STMO" = "STBO-FE-MO", 
                          "STPR" = "STBO-PR-PC", 
                          "STPC" = "STBO-PR-PC",
                          "STFO" = "STBO-FO-RA", 
                          "STRA" = "STBO-FO-RA", 
                          "STBG" = "STBG-BI-SO", 
                          "STBI" = "STBG-BI-SO", 
                          "STSO" = "STBG-BI-SO", 
                          "STLO" = "STBG-LO-MI", 
                          "STMI" = "STBG-LO-MI", 
                          "STCR" = "STBG-CR-MN", 
                          "STMN" = "STBG-CR-MN"), 
         
         dipartimento = recode(reparto, "STBO-FE-MO" = "Area Territoriale Emilia Romagna", 
                               "STBO-PR-PC" = "Area Territoriale Emilia Romagna", 
                               "STBO-FO-RA" = "Area Territoriale Emilia Romagna", 
                               "STBO-PR-PC" = "Area Territoriale Emilia Romagna", 
                               "STRE" = "Area Territoriale Emilia Romagna", 
                               "STBG-BI-SO" = "Area Territoriale Lombardia", 
                               "STBG-LO-MI" = "Area Territoriale Lombardia", 
                               "STBG-CR-MN" = "Area Territoriale Lombardia", 
                               "STPV" = "Area Territoriale Lombardia", 
                               "STBS" = "Area Territoriale Lombardia", 
                               "RPP" = "Dipartimento Sicurezza Alimentare", 
                               "RCABO" = "Dipartimento Sicurezza Alimentare", 
                               "RCA" = "Dipartimento Sicurezza Alimentare", 
                               "RCAM" = "Dipartimento Sicurezza Alimentare", 
                               "RVIR" = "Dipartimento Tutela Salute Animale", 
                               "RVVPB" = "Dipartimento Tutela Salute Animale", 
                               "RTBA" =  "Dipartimento Tutela Salute Animale", 
                               "RPCMB" = "Dipartimento Tutela Salute Animale"
         )
  )  






dtC <- obftec %>% 
  mutate(obcod = paste("OB", seq(1:nrow(.)))) %>% 
  pivot_longer(3:27, names_to = "struttura", values_to = "FTEC") %>% 
  mutate(reparto = recode(struttura, "STBO" = "STBO-FE-MO", 
                          "STFE" = "STBO-FE-MO", 
                          "STMO" = "STBO-FE-MO", 
                          "STPR" = "STBO-PR-PC", 
                          "STPC" = "STBO-PR-PC",
                          "STFO" = "STBO-FO-RA", 
                          "STRA" = "STBO-FO-RA", 
                          "STBG" = "STBG-BI-SO", 
                          "STBI" = "STBG-BI-SO", 
                          "STSO" = "STBG-BI-SO", 
                          "STLO" = "STBG-LO-MI", 
                          "STMI" = "STBG-LO-MI", 
                          "STCR" = "STBG-CR-MN", 
                          "STMN" = "STBG-CR-MN"), 
         
         dipartimento = recode(reparto, "STBO-FE-MO" = "Area Territoriale Emilia Romagna", 
                               "STBO-PR-PC" = "Area Territoriale Emilia Romagna", 
                               "STBO-FO-RA" = "Area Territoriale Emilia Romagna", 
                               "STBO-PR-PC" = "Area Territoriale Emilia Romagna", 
                               "STRE" = "Area Territoriale Emilia Romagna", 
                               "STBG-BI-SO" = "Area Territoriale Lombardia", 
                               "STBG-LO-MI" = "Area Territoriale Lombardia", 
                               "STBG-CR-MN" = "Area Territoriale Lombardia", 
                               "STPV" = "Area Territoriale Lombardia", 
                               "STBS" = "Area Territoriale Lombardia", 
                               "RPP" = "Dipartimento Sicurezza Alimentare", 
                               "RCABO" = "Dipartimento Sicurezza Alimentare", 
                               "RCA" = "Dipartimento Sicurezza Alimentare", 
                               "RCAM" = "Dipartimento Sicurezza Alimentare", 
                               "RVIR" = "Dipartimento Tutela Salute Animale", 
                               "RVVPB" = "Dipartimento Tutela Salute Animale", 
                               "RTBA" =  "Dipartimento Tutela Salute Animale", 
                               "RPCMB" = "Dipartimento Tutela Salute Animale"
         )
  )  

dtx <- dtD %>% 
  right_join(dtC,  by = c( "dipartimento", "reparto", "struttura",   "obcod"))%>% 
  select(obcod, "Obiettivo" = Obiettivo.x, "Valorizzazione"= Valorizzazione.x, "Dipartimento"=dipartimento, 
         "Reparto" = reparto, "Struttura"= struttura, FTED, FTEC )

dtx %>% 
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
