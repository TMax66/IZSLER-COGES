library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")
library("stringr")

#### PREPARAZIONE DATI ####

### dati di attività (origine da cartella dati attività 2019 vedi in scripts attività.R)####


### dati presenze matricole 2019 ####
hwd <- read_excel(here("programmazione", "data", "raw", "PresenzePersonale2019_12Ott2020.xlsx"))

hwd$CDC <- ifelse(hwd$CodiceCDC %in% c(5502, 5501), "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", hwd$CDC)

hwd <- hwd %>% 
  filter(is.na(CodiceProgetto)) %>% 
  mutate(Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
                                "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
                                "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
                                "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
                                "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
                                "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
                                "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
                                "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
                                "Controllo alimenti e trasformazioni" = "Dipartimento Sicurezza Alimentare", 
                                "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
                                "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
                                "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
                                "U.O. GESTIONE SERVIZI STRUMENTALI" = "Dipartimento Amministrativo", 
                                "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Dipartimento Amministrativo", 
                                "U.O. AFFARI GENERALI E LEGALI" = "Dipartimento Amministrativo"
                                )) %>% 
  mutate(Reparto = recode(Reparto, "Controllo alimenti e trasformazioni" = "REPARTO CONTROLLO ALIMENTI")) %>% 
  select(Dipartimento, Reparto, "Laboratorio" = CDC, Matricola, Mese, Minuti ) %>% 
  mutate(Laboratorio = recode(Laboratorio, "Tecnologia Acidi Nucleici: produzione" = "REPARTO CONTROLLO ALIMENTI", 
                              "Microbiologia: produzione" = "REPARTO CONTROLLO ALIMENTI",
                              "COSTI COMUNI REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "LABORATORIO PRODUZIONE TERRENI", 
                              "LABORATORIO LATTE" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                              "LABORATORIO DIAGNOSTICA GENERALE, SIEROLOGIA, BIOLOGIA MOLECOLARE E MICROBIOLOGIA" = "SEDE TERRITORIALE DI PIACENZA - PARMA")) %>% 
  group_by(Dipartimento, Reparto, Laboratorio, Matricola) %>% 
  summarise(hworked = sum(Minuti/60)) %>% 
  filter(str_detect(Dipartimento, paste(c("Dipartimento", "Area"),collapse = '|')) ) %>% 
  filter(Dipartimento != "Dipartimento Amministrativo")


###dati conversione matricole gru-sigma##
grusigma <- read_excel(here("programmazione", "data", "raw", "MatricoleSigmaGRU.xlsx"))
grusigma <- grusigma %>% 
  select("matricola" = Matricola, "Matricola" = MatricolaSigma)
### dati dei presenti nel 2019 -anagrafica con dati contrattuali ####
anag19 <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))

anag19 %>% 
  select("matricola" = CDMATR, 
         "sesso" = SESSO, 
         "dtnasc" = DTNASC, 
         "categoria" = DEMANSP3, 
         "hperc" = PCGIUR, 
         "contratto" = DECOMP) %>% 
  mutate(hcontr = ifelse( contratto == "COMPARTO SSN", (36*hperc)/100, (38*hperc)/100)) %>% 
  # filter(contratto == "COMPARTO SSN") %>%
  left_join(grusigma, by = "matricola") %>% 
  right_join(hwd, by = "Matricola" ) %>% 
  group_by(Dipartimento, Reparto, Laboratorio) %>% 
  summarise(hworked= sum(hworked), 
            hprev = sum(hcontr*45.6)) %>% 
  saveRDS(., file = here("programmazione", "data", "processed", "hwd19.rds"))

### Dati di attività e ricavi 2019###

riepilogo <- read_excel( sheet = "riepilogo", here("report attività 2019", "data", "raw", "dati.xls"))
reparti <- read_excel( sheet = "reparti", here("report attività 2019", "data", "raw", "dati.xls"))
costi<- read_excel(sheet = "Reparti Formato Long", here(  "report attività 2019", "data", "raw", "costi personale.xls"))

r<-reparti %>% 
  group_by(Reparto, Laboratorio) %>% 
  summarise(esami=round(sum(n.esami),0), ricavi=round(sum(valore),0)) %>% 
  saveRDS(., file = here("programmazione", "data", "processed", "esamiricavi2019.rds"))

riepilogo %>%
  select(Reparto,`Attività Interna`) %>%
  drop_na(`Attività Interna`) %>% 
  saveRDS(., file = here("programmazione", "data", "processed", "ainterna19.rds"))

riepilogo %>%
  select(Reparto,`Vendita Prodotti`) %>%
  drop_na(`Vendita Prodotti`) %>% 
  saveRDS(., file = here("programmazione", "data", "processed", "vprodotti.rds"))

costi %>% select(3:5) %>% 
  unique() %>% 
  saveRDS(., file = here("programmazione", "data", "processed", "costip2019.rds"))




