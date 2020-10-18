library(here)
library(tidyverse)

## carico i dati rds###

hwd19 <- readRDS( here("programmazione", "data", "processed", "hwd19.rds"))

att19 <- readRDS( here("programmazione", "data", "processed", "esamiricavi2019.rds"))

att19 %>% 
  mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA", 
                          "TECNOLOGIE BIOLOGICHE APPLICATE"= "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                          "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA", 
                          "CHIMICO DEGLI ALIMENTI E DEI MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                          "CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
                          "CONTROLLO ALIMENTI" = "REPARTO CONTROLLO ALIMENTI", 
                          "BERGAMO - BINAGO - SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                          "BRESCIA" = "SEDE TERRITORIALE DI BRESCIA", 
                          "PAVIA" = "SEDE TERRITORIALE DI PAVIA", 
                          "CREMONA - MANTOVA" =  "SEDE TERRITORIALE DI CREMONA - MANTOVA", 
                          "LODI - MILANO" = "SEDE TERRITORIALE DI LODI - MILANO", 
                          "FORLI' - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA", 
                          "BOLOGNA - MODENA - FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
                          "PIACENZA - PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                          "REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA" )) %>% 
  filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
  mutate(Laboratorio = recode(Laboratorio,"Bologna" = "SEDE TERRITORIALE DI BOLOGNA", 
                              "Modena" = "SEDE TERRITORIALE DI MODENA", 
                              "Ferrara" = "SEDE TERRITORIALE DI FERRARA", 
                              "Bergamo" = "SEDE TERRITORIALE DI BERGAMO", 
                              "Binago" = "SEDE TERRITORIALE DI BINAGO", 
                              "Sondrio" = "SEDE TERRITORIALE DI SONDRIO", 
                              "Brescia" = "SEDE TERRITORIALE DI BRESCIA")) %>% 
  View()
  


  
