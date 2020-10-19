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
                          "CHIMICO DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
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
                              "Brescia" = "SEDE TERRITORIALE DI BRESCIA", 
                              "Chimico degli Alimenti (Bologna)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
                              "Chimica Applicata alle Tecnologie Alimentari" = "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI",
                              "Contaminanti Ambientali" = "LABORATORIO CONTAMINANTI AMBIENTALI", 
                              "Mangimi e Tossicologia" = "LABORATORIO MANGIMI E TOSSICOLOGIA", 
                              "Residui" = "LABORATORIO RESIDUI", 
                              "Controllo Alimenti" = "REPARTO CONTROLLO ALIMENTI", 
                              "Cremona" = "SEDE TERRITORIALE DI CREMONA", 
                              "Mantova" = "SEDE TERRITORIALE DI MANTOVA", 
                              "Forlì" = "SEDE TERRITORIALE DI FORLÌ", 
                              "Ravenna" = "SEDE TERRITORIALE DI RAVENNA", 
                              "Lodi" = "SEDE TERRITORIALE DI LODI", 
                              "Milano" = "SEDE TERRITORIALE DI MILANO", 
                              "Pavia" = "SEDE TERRITORIALE DI PAVIA", 
                              "Parma" = "SEDE TERRITORIALE DI PARMA", 
                              "Piacenza" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                              "Reggio Emilia" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
                              "Benessere Animale, Biochimica Clinica, Immunologia Veterinaria e Stabulari" = "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI", 
                              "Controllo di Prodotti Biologici, Farmaceutici e Convalida dei Processi Produttivi" = "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI", 
                              "Produzione Terreni" = "LABORATORIO PRODUZIONE TERRENI", 
                              "Produzione Vaccini e Reagenti" = "LABORATORIO PRODUZIONE VACCINI E REAGENTI", 
                              "Produzione Primaria" = "REPARTO PRODUZIONE PRIMARIA", 
                              "Analisi Genomiche, Diagnostica Molecolare, OGM" = "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM", 
                              "Batteriologia Specializzata" = "LABORATORIO BATTERIOLOGIA SPECIALIZZATA", 
                              "Colture Cellulari, Biobanca" = "LABORATORIO COLTURE CELLULARI, BIOBANCA", 
                              "Proteomica" = "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE", 
                              "Virologia" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA", 
                              "Virus Vescicolari e Produzioni Biotecnologiche" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE")) %>% 
  left_join(., hwd19, by = c("Reparto", "Laboratorio")) 
 

  


  
