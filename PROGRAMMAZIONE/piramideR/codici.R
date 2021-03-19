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


anag19 <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))## anagrafica del 2019 (da qui prendo le matricole)
grusigma <- read_excel(here("programmazione", "data", "raw", "MatricoleSigmaGRU.xlsx"))
grusigma <- grusigma %>% 
  select("matricola" = Matricola, "Matricola" = MatricolaSigma)



anag19 %>%
  select("matricola" = CDMATR,
         "sesso" = SESSO,
         "dtnasc" = DTNASC,
         "categoria" = DEMANSP3,
         "hperc" = PCGIUR,
         "contratto" = DECOMP) %>%
  left_join(grusigma, by = "matricola") %>%
  mutate(matunique = !duplicated(matricola)) %>%
  filter(matunique == "TRUE") %>%
  right_join(hwd, by = "Matricola" ) %>%
  filter(contratto == "DIRIGENZA") %>%
  select(matricola,Dipartimento, Reparto, Laboratorio, hcontr, hworked ) %>%
  saveRDS(., file = here("programmazione", "data", "processed", "matrperpubb.rds"))







# hwd <- read_excel(here("programmazione", "data", "raw", "PresenzePersonale2019_12Ott2020.xlsx"))
# 
# 
# 
# 
# hwd$CDC <- ifelse(hwd$CodiceCDC %in% c(5502, 5501), "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", hwd$CDC)
# hwd <- hwd %>% 
#   filter(is.na(CodiceProgetto)) %>% 
#   mutate(Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
#                                 "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
#                                 "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
#                                 "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
#                                 "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
#                                 "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
#                                 "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
#                                 "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
#                                 "Controllo alimenti e trasformazioni" = "Dipartimento Sicurezza Alimentare", 
#                                 "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
#                                 "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
#                                 "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
#                                 "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
#                                 "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
#                                 "U.O. GESTIONE SERVIZI STRUMENTALI" = "Dipartimento Amministrativo", 
#                                 "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Dipartimento Amministrativo", 
#                                 "U.O. AFFARI GENERALI E LEGALI" = "Dipartimento Amministrativo"
#   )) %>% 
#   mutate(Reparto = recode(Reparto, "Controllo alimenti e trasformazioni" = "REPARTO CONTROLLO ALIMENTI")) %>% 
#   select(Dipartimento, Reparto, "Laboratorio" = CDC, Matricola, Mese, Minuti ) %>% 
#   mutate(Laboratorio = recode(Laboratorio, "Tecnologia Acidi Nucleici: produzione" = "REPARTO CONTROLLO ALIMENTI", 
#                               "Microbiologia: produzione" = "REPARTO CONTROLLO ALIMENTI",
#                               "COSTI COMUNI REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "LABORATORIO PRODUZIONE TERRENI", 
#                               "LABORATORIO LATTE" = "SEDE TERRITORIALE DI PIACENZA", 
#                               "LABORATORIO DIAGNOSTICA GENERALE, SIEROLOGIA, BIOLOGIA MOLECOLARE E MICROBIOLOGIA" = "SEDE TERRITORIALE DI PIACENZA")) %>% 
#   group_by(Dipartimento, Reparto, Laboratorio, Matricola) %>% 
#   summarise(hworked = sum(Minuti/60)) %>% 
#   filter(str_detect(Dipartimento, paste(c("Dipartimento", "Area"),collapse = '|')) ) %>% 
#   filter(Dipartimento != "Dipartimento Amministrativo")
# 
# 























































###Pubblicazioni####
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

repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script


pubblicazioni %>% 
  right_join(matricole, by = "autore") %>%  
  filter(!is.na(nr)) %>% 
  select(nr, reparto, autore, tipologia, matricola, autori, titinglese, datibiblio,`TITOLO RIVISTA`, convegno, titoriginale, impf ) %>% 
  right_join(repMat, by = "matricola") %>% 
  filter(!is.na(nr)) %>% 
  saveRDS(here("programmazione", "shinyapp", "ricerca.rds"))

ricerca <- readRDS(here("programmazione", "shinyapp-in-produzione",  "ricerca.rds"))
ricerca <- ricerca %>% 
  mutate(IF = ifelse(tipologia == "IF ; Int" | tipologia == "IF",  "IF", NA), 
         INT = ifelse(tipologia == "IF ; Int" | tipologia == "Int",  "Int", NA ), 
         NAZ = ifelse(tipologia == "Naz", "Naz", NA), 
         Oth = ifelse(tipologia == "Others" , "Others", NA))

 

ricerca %>%
  filter(IF == IF) %>%
  filter (! duplicated(nr)) %>% 
  group_by(Dipartimento, Reparto) %>% 
  summarise(IF= mean(impf, na.rm = TRUE),
            minIF = min(impf, na.rm = TRUE), 
            maxIF = max(impf, na.rm = TRUE), 
            MIF = median(impf, na.rm = TRUE),
            sumIF = sum(impf, na.rm = TRUE), 
            N.pub = n()) %>% 
  arrange(desc(MIF))


##Progetti di ricerca####

pr <- read_excel(here("programmazione", "data", "raw", "DatiProgettiUO.xlsx"))
#repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script

## andare allo script di preparazione repMat ed eliminare la variabile Laboratorio che duplica le colonne dopo il join con pr

anag19 <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))## anagrafica del 2019 (da qui prendo le matricole)
grusigma <- read_excel(here("programmazione", "data", "raw", "MatricoleSigmaGRU.xlsx"))
grusigma <- grusigma %>% 
  select("matricola" = Matricola, "Matricola" = MatricolaSigma)


repMat <- anag19 %>%
  select("matricola" = CDMATR,
         "sesso" = SESSO,
         "dtnasc" = DTNASC,
         "categoria" = DEMANSP3,
         "hperc" = PCGIUR,
         "contratto" = DECOMP) %>%
  left_join(grusigma, by = "matricola") %>% 
  mutate(matunique = !duplicated(matricola)) %>%
  filter(matunique == "TRUE") %>%
  right_join(hwd, by = "Matricola" ) %>% 
  select(matricola,Dipartimento, Reparto )

###calcola per dipartimento/reparto/tipologia e codiceprg il numero di u.o. partecipanti e il budget

pr %>% select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2019-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2019-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2019-12-31"), "Concluso", "Aperto")) %>%
  left_join(repMat, by = c("MatrRSUO" = "matricola")) %>% View()
  filter(Statoanno == "Aperto") %>% 
       group_by(Dipartimento) %>% 
  summarise(Bdg = sum(Budget), 
            MBdg = mean(Budget, na.rm = T),
            MdBdg = median(Budget, na.rm = T), 
            mdBdg = min(Budget, na.rm = T), 
            mxBdg = max(Budget, na.rm = T), 
            "Progetti di Ricerca"=nlevels(factor(Codice)))%>% View()
       filter(!is.na(Dipartimento))

       
       
       pr %>%
         group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
         summarise(Budget = sum(Budget), nUO = n()) %>% 
         ungroup() %>% 
         group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
         summarise(Durata = as.numeric(DataFine-DataInizio), 
                   R = as.numeric(date("2019-12-31")-date(DataInizio)), 
                   Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
                   Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
         mutate(DataInizio = as.character(DataInizio), 
                DataFine = as.character(DataFine)) %>% 
         arrange(Realizzazione) %>% 
         select(-R, -Durata) %>% View()
       