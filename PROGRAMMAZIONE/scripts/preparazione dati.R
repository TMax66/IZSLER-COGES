library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")
library("stringr")
library("janitor")
library("flextable")

#### PREPARAZIONE DATI ####

 
### dati presenza matricole 2019 - CARTELLINO ####
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
                              "LABORATORIO LATTE" = "SEDE TERRITORIALE DI PIACENZA", 
                              "LABORATORIO DIAGNOSTICA GENERALE, SIEROLOGIA, BIOLOGIA MOLECOLARE E MICROBIOLOGIA" = "SEDE TERRITORIALE DI PIACENZA")) %>% 
  group_by(Dipartimento, Reparto, Laboratorio, Matricola) %>% 
  summarise(hworked = sum(Minuti/60)) %>% 
  filter(str_detect(Dipartimento, paste(c("Dipartimento", "Area"),collapse = '|')) ) %>% 
  filter(Dipartimento != "Dipartimento Amministrativo")

###dati conversione matricole gru-sigma####
grusigma <- read_excel(here("programmazione", "data", "raw", "MatricoleSigmaGRU.xlsx"))
grusigma <- grusigma %>% 
  select("matricola" = Matricola, "Matricola" = MatricolaSigma)

### dati-anagrafica con dati contrattuali degli attivi nel 2019 ####
anag19 <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))
anag19 %>% 
  select("matricola" = CDMATR, 
         "sesso" = SESSO, 
         "dtnasc" = DTNASC, 
         "categoria" = DEMANSP3, 
         "hperc" = PCGIUR, 
         "contratto" = DECOMP) %>% 
  mutate(contratto = recode(contratto, "DIRIGENZA MEDICO/VETERINARIA SSN" = "DIRIGENZA", 
                           "DIRIGENZA S.P.T.A. SSN" = "DIRIGENZA", 
                           "COMPARTO SSN" = "COMPARTO")) %>% 
  mutate(hcontr = ifelse( contratto == "COMPARTO", (36*hperc)/100, (38*hperc)/100)) %>% 
  
  # filter(contratto == "COMPARTO SSN") %>%
  left_join(grusigma, by = "matricola") %>%  
  mutate(matunique = !duplicated(matricola)) %>%  
  filter(matunique == "TRUE") %>% 
  right_join(hwd, by = "Matricola" ) %>%  
  group_by(Dipartimento, Reparto, Laboratorio, contratto) %>% 
  summarise(hworked= sum(hworked), 
            hprev = sum(hcontr*47.4)) %>%
  saveRDS(., file = here("programmazione", "data", "processed", "hwd19.rds"))

 
 ###dataset per link a pubblicazioni####
 anag19 %>%
   select("matricola" = CDMATR,
          "sesso" = SESSO,
          "dtnasc" = DTNASC,
          "categoria" = DEMANSP3,
          "hperc" = PCGIUR,
          "contratto" = DECOMP) %>%
   mutate(contratto = recode(contratto, "DIRIGENZA MEDICO/VETERINARIA SSN" = "DIRIGENZA",
                             "DIRIGENZA S.P.T.A. SSN" = "DIRIGENZA",
                             "COMPARTO SSN" = "COMPARTO")) %>%
   mutate(hcontr = ifelse( contratto == "COMPARTO", (36*hperc)/100, (38*hperc)/100)) %>%

   # filter(contratto == "COMPARTO SSN") %>%
   left_join(grusigma, by = "matricola") %>%
   mutate(matunique = !duplicated(matricola)) %>%
   filter(matunique == "TRUE") %>%
   right_join(hwd, by = "Matricola" ) %>%
   filter(contratto == "DIRIGENZA") %>%
   select(matricola,Dipartimento, Reparto, Laboratorio, hcontr, hworked ) %>%
   saveRDS(., file = here("programmazione", "data", "processed", "matrperpubb.rds"))
   
   


### Dati di attività e ricavi 2019###

riepilogo <- read_excel( sheet = "riepilogo", here("report2019", "data", "raw", "dati.xls"))
#reparti <- read_excel( sheet = "reparti", here("report attività 2019", "data", "raw", "dati.xls"))
# costi<- read_excel(sheet = "Reparti Formato Long", here(  "report attività 2019", "data", "raw", "costi personale.xls"))
# 
# r<-reparti %>% 
#   group_by(Reparto, Laboratorio) %>% 
#   summarise(esami=round(sum(n.esami),0), ricavi=round(sum(valore),0)) %>% 
#   saveRDS(., file = here("programmazione", "data", "processed", "esamiricavi2019.rds"))
# 
riepilogo %>%
  select(Reparto,`Attività Interna`) %>%
  drop_na(`Attività Interna`) %>%
  saveRDS(., file = here("programmazione", "data", "processed", "ainterna19.rds"))

riepilogo %>%
  select(Reparto,`Vendita Prodotti`) %>%
  drop_na(`Vendita Prodotti`) %>%
  saveRDS(., file = here("programmazione", "data", "processed", "vprodotti.rds"))
# 
# costi %>% select(3:5) %>% 
#   unique() %>% 
#   saveRDS(., file = here("programmazione", "data", "processed", "costip2019.rds"))
# 

## carico i dati rds###

#hwd19 <- readRDS( here("programmazione", "data", "processed", "hwd19.rds"))

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
                              "Piacenza" = "SEDE TERRITORIALE DI PIACENZA", 
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
  left_join(., hwd19, by = c("Reparto", "Laboratorio")) %>% 
   mutate("FTE-previsto" = ifelse(contratto == "DIRIGENZA", hprev/(38*47.4), hprev/(36*47.4)),
         "FTE-reale" = ifelse(contratto == "DIRIGENZA", hworked/(38*47.4), hworked/(36*47.4))) %>% 
         # "%tempo-utilizzato" = 100*(hworked/hprev),
         # "tempo-medio esame" = hworked/esami,
         # "RxFTEr" = ricavi/`FTE-reale`) %>%
  #saveRDS(., file = here("programmazione", "data", "processed", "dati.rds"))
  saveRDS(., file = here("programmazione", "shinyapp",  "dati.rds"))








####carico i dati x collegare i ricavi da vp e ai al dataset  complessivo####

vp <- readRDS( here("programmazione", "data", "processed", "vprodotti.rds"))
ai <- readRDS( here("programmazione", "data", "processed", "ainterna19.rds"))
dati <- readRDS( here("programmazione", "data", "processed", "dati.rds"))


vp %>% 
  filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
  mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA",
                          "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "CONTROLLO ALIMENTI" = "REPARTO CONTROLLO ALIMENTI",
                          "PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
                          "CHIMICO DEGLI ALIMENTI E DEI MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                          "BERGAMO - BINAGO - SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "CREMONA - MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "PAVIA" = "SEDE TERRITORIALE DI PAVIA",
                          "LODI - MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "BRESCIA" = "SEDE TERRITORIALE DI BRESCIA", 
                          "BOLOGNA - MODENA - FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "FORLI' - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "PIACENZA - PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA"),
         Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
                                                               "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
                                                               "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
                                                               "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
                                                               "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
                                                               "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
                                                               "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
                                                               "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
                                                               "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
                                                               "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
                                                               "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
                                                               "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
                                                               "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
                                                               "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
                                                               "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
                                                               "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
                                                               "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna")) %>% 
  # group_by(Dipartimento) %>% 
  # summarise(VP = round(sum(`Vendita Prodotti`), 0)) %>% 
  saveRDS(., file = here("programmazione", "shinyapp", "vp.rds"))


 ai %>% 
  filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
  mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA",
                          "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "CONTROLLO ALIMENTI" = "REPARTO CONTROLLO ALIMENTI",
                          "PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
                          "CHIMICO DEGLI ALIMENTI E DEI MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                          "BERGAMO - BINAGO - SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "CREMONA - MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "PAVIA" = "SEDE TERRITORIALE DI PAVIA",
                          "LODI - MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "BRESCIA" = "SEDE TERRITORIALE DI BRESCIA", 
                          "BOLOGNA - MODENA - FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "FORLI' - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "PIACENZA - PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA"), 
         Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
                                "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
                                "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
                                "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
                                "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
                                "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
                                "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
                                "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
                                "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
                                "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
                                "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna"))%>% 
  # group_by(Dipartimento) %>% 
  # summarise(AI = round(sum(`Attività Interna`),0)) %>% 
   saveRDS(., file = here("programmazione", "shinyapp", "ai.rds"))



## dati programmazione da scheda budget####

 obfted <- read_excel(here("programmazione", "data", "raw", "obiettiviXSB.xlsx"), sheet = "FTEDdipsan")
 obftec <- read_excel(here("programmazione", "data", "raw", "obiettiviXSB.xlsx"), sheet = "FTECdipsan")
 
 dtD <- obfted %>% 
   mutate(obcod = paste("OB", seq(1:nrow(.)))) %>%
   pivot_longer(3:37, names_to = "struttura", values_to = "FTED") %>% 
   mutate(reparto = recode(struttura, "STBO" = "STBO-FE-MO", 
                           "STFE" = "STBO-FE-MO", 
                           "STMO" = "STBO-FE-MO", 
                           "STPR" = "STPR-PC", 
                           "STPC" = "STPR-PC",
                           "STFO" = "STFO-RA", 
                           "STRA" = "STFO-RA", 
                           "STBG" = "STBG-BI-SO", 
                           "STBI" = "STBG-BI-SO", 
                           "STSO" = "STBG-BI-SO", 
                           "STLO" = "STLO-MI", 
                           "STMI" = "STLO-MI", 
                           "STCR" = "STCR-MN", 
                           "STMN" = "STCR-MN"), 
          
          dipartimento = recode(reparto, "STBO-FE-MO" = "Area Territoriale Emilia Romagna", 
                                "STPR-PC" = "Area Territoriale Emilia Romagna", 
                                "STFO-RA" = "Area Territoriale Emilia Romagna", 
                                # "STBO-PR-PC" = "Area Territoriale Emilia Romagna", 
                                "STRE" = "Area Territoriale Emilia Romagna", 
                                "STBG-BI-SO" = "Area Territoriale Lombardia", 
                                "STLO-MI" = "Area Territoriale Lombardia", 
                                "STCR-MN" = "Area Territoriale Lombardia", 
                                "STPV" = "Area Territoriale Lombardia", 
                                "STBS" = "Area Territoriale Lombardia", 
                                "RPP" = "Dipartimento Sicurezza Alimentare", 
                                "RCABO" = "Dipartimento Sicurezza Alimentare", 
                                "RCA" = "Dipartimento Sicurezza Alimentare", 
                                "RCAM" = "Dipartimento Sicurezza Alimentare", 
                                "RVIR" = "Dipartimento Tutela Salute Animale", 
                                "RVVPB" = "Dipartimento Tutela Salute Animale", 
                                "RTBA" =  "Dipartimento Tutela Salute Animale", 
                                "RPCMB" = "Dipartimento Tutela Salute Animale", 
                                "AREG" = "Direzione Sanitaria", 
                                "SORVEPIDEM" = "Direzione Sanitaria", 
                                "GESTCENT" = "Direzione Sanitaria", 
                                "FORMAZIONE" = "Direzione Sanitaria", 
                                "SAQ" = "Direzione Sanitaria", 
                                "AFFGENLEG" = "Dipartimento Amministrativo", 
                                "GESTRISUM" = "Dipartimento Amministrativo", 
                                "UOTECPAT" = "Dipartimento Amministrativo", 
                                "PROVV" = "Dipartimento Amministrativo", 
                                "SERVCONT" = "Dipartimento Amministrativo"
          )
   )   
 
 
 
 
 
 dtC <- obftec %>% 
   mutate(obcod = paste("OB", seq(1:nrow(.)))) %>% 
   pivot_longer(3:37, names_to = "struttura", values_to = "FTEC") %>% 
   mutate(reparto = recode(struttura, "STBO" = "STBO-FE-MO", 
                           "STFE" = "STBO-FE-MO", 
                           "STMO" = "STBO-FE-MO", 
                           "STPR" = "STPR-PC", 
                           "STPC" = "STPR-PC",
                           "STFO" = "STFO-RA", 
                           "STRA" = "STFO-RA", 
                           "STBG" = "STBG-BI-SO", 
                           "STBI" = "STBG-BI-SO", 
                           "STSO" = "STBG-BI-SO", 
                           "STLO" = "STLO-MI", 
                           "STMI" = "STLO-MI", 
                           "STCR" = "STCR-MN", 
                           "STMN" = "STCR-MN"), 
          
          dipartimento = recode(reparto, "STBO-FE-MO" = "Area Territoriale Emilia Romagna", 
                                "STPR-PC" = "Area Territoriale Emilia Romagna", 
                                "STFO-RA" = "Area Territoriale Emilia Romagna", 
                                # "STBO-PR-PC" = "Area Territoriale Emilia Romagna", 
                                "STRE" = "Area Territoriale Emilia Romagna", 
                                "STBG-BI-SO" = "Area Territoriale Lombardia", 
                                "STLO-MI" = "Area Territoriale Lombardia", 
                                "STCR-MN" = "Area Territoriale Lombardia", 
                                "STPV" = "Area Territoriale Lombardia", 
                                "STBS" = "Area Territoriale Lombardia", 
                                "RPP" = "Dipartimento Sicurezza Alimentare", 
                                "RCABO" = "Dipartimento Sicurezza Alimentare", 
                                "RCA" = "Dipartimento Sicurezza Alimentare", 
                                "RCAM" = "Dipartimento Sicurezza Alimentare", 
                                "RVIR" = "Dipartimento Tutela Salute Animale", 
                                "RVVPB" = "Dipartimento Tutela Salute Animale", 
                                "RTBA" =  "Dipartimento Tutela Salute Animale", 
                                "RPCMB" = "Dipartimento Tutela Salute Animale",
                                "AREG" = "Direzione Sanitaria", 
                                "SORVEPIDEM" = "Direzione Sanitaria", 
                                "GESTCENT" = "Direzione Sanitaria", 
                                "FORMAZIONE" = "Direzione Sanitaria", 
                                "SAQ" = "Direzione Sanitaria", 
                                "AFFGENLEG" = "Dipartimento Amministrativo", 
                                "GESTRISUM" = "Dipartimento Amministrativo", 
                                "UOTECPAT" = "Dipartimento Amministrativo", 
                                "PROVV" = "Dipartimento Amministrativo", 
                                "SERVCONT" = "Dipartimento Amministrativo"
          )
   )  

 dtD %>% 
   right_join(dtC,  by = c( "dipartimento", "reparto", "struttura",   "obcod"))%>% 
   select(obcod, "Obiettivo" = Obiettivo.x, "Valorizzazione"= Valorizzazione.x, "Dipartimento"=dipartimento, 
          "Reparto" = reparto, "Struttura"= struttura, FTED, FTEC ) %>% 
   mutate(Dipartimento = factor(Dipartimento, levels = c("Direzione Sanitaria", 
                                                         "Dipartimento Amministrativo", 
                                                         "Dipartimento Sicurezza Alimentare", 
                                                         "Dipartimento Tutela Salute Animale", 
                                                         "Area Territoriale Lombardia", 
                                                         "Area Territoriale Emilia Romagna"))) %>% 
  saveRDS(., file = here("programmazione", "shinyapp-in-produzione", "datiSB.rds"))

   
 
  

 

## full time equivalenti al 1 gennaio 2021####
dati21 <- read_excel(here("programmazione", "data", "raw", "presenze2021.xlsx"))


presenze21 <- dati21 %>% 
  mutate(REPARTO = recode(REPARTO, 
                          "SEDE TERRITORIALE DI BERGAMO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "SEDE TERRITORIALE DI BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                          "SEDE TERRITORIALE DI SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                          "SEDE TERRITORIALE DI CREMONA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "SEDE TERRITORIALE DI MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "SEDE TERRITORIALE DI LODI" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "SEDE TERRITORIALE DI MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "SEDE TERRITORIALE DI BOLOGNA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "SEDE TERRITORIALE DI MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "SEDE TERRITORIALE DI FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "SEDE TERRITORIALE DI FORLÌ" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "SEDE TERRITORIALE DI RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "SEDE TERRITORIALE DI PIACENZA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "SEDE TERRITORIALE DI PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                          "LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "LABORATORIO RESIDUI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                          "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                          "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                          "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA",
                          "LABORATORIO PRODUZIONE VACCINI E REAGENTI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                          "LABORATORIO COLTURE CELLULARI, BIOBANCA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE"
  ),
  Dipartimento = recode (REPARTO, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
                         "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
                         "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
                         "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
                         "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
                         "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
                         "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
                         "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
                         "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
                         "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
                         "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
                         "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
                         "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
                         "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
                         "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "Direzione Sanitaria", 
                         "CONTROLLO DI GESTIONE" = "Direzione Generale", 
                         "DIREZIONE AMMINISTRATIVA" = "Direzione Ammninistrativa", 
                         "DIREZIONE GENERALE" = "Direzione Generale", 
                         "DIREZIONE SANITARIA" = "Direzione Sanitaria",
                         "FORMAZIONE E BIBLIOTECA" = "Direzione Sanitaria", 
                         "GARE CONTRATTI PER ACQUISTO DI BENI E SERVIZI, MAGAZZINO E VENDITE, UFFICIO SERVIZI" = "Direzione Ammninistrativa",
                         "GESTIONE CENTRALIZZATA DELLE RICHIESTE" = "Direzione Sanitaria", 
                         "PROGETTAZIONE E DIREZIONE LAVORI MANUTENZIONI" = "Direzione Ammninistrativa", 
                         "PROGETTI DI RICERCA" = "Direzione Generale", 
                         "SERVIZIO ASSICURAZIONE QUALITA'" = "Direzione Generale",
                         "SISTEMI INFORMATIVI" = "Direzione Generale",
                         "SORVEGLIANZA EPIDEMIOLOGICA EMILIA ROMAGNA" = "Direzione Sanitaria", 
                         "SORVEGLIANZA EPIDEMIOLOGICA LOMBARDIA" = "Direzione Sanitaria", 
                         "U.O. AFFARI GENERALI E LEGALI" = "Direzione Ammninistrativa", 
                         "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Direzione Ammninistrativa", 
                         "U.O. GESTIONE SERVIZI CONTABILI" = "Direzione Ammninistrativa", 
                         "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE" = "Dipartimento Tutela e  Salute Animale"
  ),
  CENTRO_DI_COSTO = recode(CENTRO_DI_COSTO, "LABORATORIO DIAGNOSTICA GENERALE, SIEROLOGIA, BIOLOGIA MOLECOLARE E MICROBIOLOGIA" = "SEDE TERRITORIALE DI PIACENZA",
                           "LABORATORIO LATTE" = "SEDE TERRITORIALE DI PIACENZA"),
  
  contr  = ifelse( Dirigente == "N", (36*`Perc Orario`)/100, (38*`Perc Orario`)/100),
  hcontr =  contr*47.4
  
  ) %>% 
  select(Dipartimento, REPARTO, CENTRO_DI_COSTO, Dirigente, contr, hcontr) %>% 
  group_by(Dipartimento, REPARTO, CENTRO_DI_COSTO, Dirigente) %>% 
  summarise(hcontr = sum(hcontr)) %>% 
  mutate(FTE = ifelse(Dirigente == "S", hcontr/(38*47.4), hcontr/(36*47.4))) %>% 
  select(-hcontr) %>% 
  pivot_wider(names_from = Dirigente, values_from = FTE)



group_by(REPARTO, CENTRO_DI_COSTO, Dirigente) %>% 
  summarise(FTE= sum(FTE))



library("writexl")

write_xlsx(presenze21, path = "presenze21.xlsx")  

MatricoleSigmaGRU <- read_excel("programmazione/data/raw/MatricoleSigmaGRU.xlsx")

matricole <- presenze21 %>% 
  left_join(MatricoleSigmaGRU, by="Matricola")  


writexl::write_xlsx(matricole, path= "matricole.xlsx")
 
# ###tabella per documento piano performance##
# dir <- dati %>% 
#   filter(contratto == "DIRIGENZA") %>% 
#   group_by(Dipartimento) %>% 
#   summarise(esami = sum(esami), 
#             ricavi = sum(ricavi),
#             FTE_d = round(sum(`FTE-reale`),1))
#   
# 
# comp <- dati %>% 
#   filter(contratto == "COMPARTO") %>% 
#   group_by(Dipartimento) %>% 
#   summarise(esami = sum(esami), 
#             ricavi = sum(ricavi),
#             FTE_c = round(sum(`FTE-reale`),1))
# 
# tabella <- dir %>% 
#   bind_cols((comp %>% 
#               select(4)), (vp %>% 
#                              select(2)), (ai %>% 
#                                             select(2))) %>% 
#   
#   mutate(RT = (ricavi+VP+AI), 
#          FTE_t = round((FTE_d+FTE_c),1)) %>% 
#   arrange(desc(esami)) %>% 
#   adorn_totals(where = "row") %>% 
#   mutate( "R-FTE" = round(RT/FTE_t,0) ) %>% 
#   select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP, 
#          "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")  
#  
# 
# ft <- flextable(tabella)
# ft <- autofit(ft)
# print(ft, preview = "docx")
# 
# 
# 
 
