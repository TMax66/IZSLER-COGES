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

##PROGETTI DI RICERCA####

anag21 <- readRDS(here("programmazione", "data", "processed", "ANAGRAFE2021.rds"))
pr <- read_excel(here("programmazione", "piramideR", "pr2020.xlsx"))

pr %>% 
  #select(-14, -15) %>% 
  mutate("Stato" = ifelse(DataFine < as.Date("2020-01-01"), "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & DataInizio <= as.Date("2020-12-31")) %>% 
  mutate("Statoanno" = ifelse(DataFine <=as.Date("2020-12-31"), "Concluso", "Aperto")) %>% 
  mutate(MatrRSUO = ifelse(is.na(MatrRSUO), MatrRS, MatrRSUO)) %>% 
  left_join(anag21, by = c("MatrRSUO" = "Matricola")) %>% View()



#repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script

## andare allo script di preparazione repMat ed eliminare la variabile Laboratorio che duplica le colonne dopo il join con pr


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
            N.pub = n()) %>% View()
 
  #arrange(desc(MIF)) %>% 
  ggplot(aes(x=Reparto, y=MIF))+
  geom_bar(stat = "identity")+
  coord_flip()


x = fct_infreq(Position)

       
       
       
       ####codice per anagrafe 2021#####
       # dati21 <- read_excel(here("programmazione", "data", "raw", "presenze2021.xlsx"))
       # 
       # 
       # anag21 <- dati21 %>% 
       #   mutate(REPARTO = recode(REPARTO, 
       #                           "SEDE TERRITORIALE DI BERGAMO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
       #                           "SEDE TERRITORIALE DI BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
       #                           "SEDE TERRITORIALE DI SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
       #                           "SEDE TERRITORIALE DI CREMONA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
       #                           "SEDE TERRITORIALE DI MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
       #                           "SEDE TERRITORIALE DI LODI" = "SEDE TERRITORIALE DI LODI - MILANO",
       #                           "SEDE TERRITORIALE DI MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
       #                           "SEDE TERRITORIALE DI BOLOGNA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
       #                           "SEDE TERRITORIALE DI MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
       #                           "SEDE TERRITORIALE DI FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
       #                           "SEDE TERRITORIALE DI FORLÌ" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
       #                           "SEDE TERRITORIALE DI RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
       #                           "SEDE TERRITORIALE DI PIACENZA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
       #                           "SEDE TERRITORIALE DI PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
       #                           "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
       #                           "LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
       #                           "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
       #                           "LABORATORIO RESIDUI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
       #                           "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
       #                           "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
       #                           "LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
       #                           "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
       #                           "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
       #                           "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA",
       #                           "LABORATORIO PRODUZIONE VACCINI E REAGENTI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
       #                           "LABORATORIO COLTURE CELLULARI, BIOBANCA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE"
       #   ),
       #   Dipartimento = recode (REPARTO, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
       #                          "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
       #                          "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
       #                          "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
       #                          "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
       #                          "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
       #                          "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
       #                          "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
       #                          "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
       #                          "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
       #                          "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
       #                          "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
       #                          "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
       #                          "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
       #                          "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
       #                          "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
       #                          "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
       #                          "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "Direzione Sanitaria", 
       #                          "CONTROLLO DI GESTIONE" = "Direzione Generale", 
       #                          "DIREZIONE AMMINISTRATIVA" = "Direzione Ammninistrativa", 
       #                          "DIREZIONE GENERALE" = "Direzione Generale", 
       #                          "DIREZIONE SANITARIA" = "Direzione Sanitaria",
       #                          "FORMAZIONE E BIBLIOTECA" = "Direzione Sanitaria", 
       #                          "GARE CONTRATTI PER ACQUISTO DI BENI E SERVIZI, MAGAZZINO E VENDITE, UFFICIO SERVIZI" = "Direzione Ammninistrativa",
       #                          "GESTIONE CENTRALIZZATA DELLE RICHIESTE" = "Direzione Sanitaria", 
       #                          "PROGETTAZIONE E DIREZIONE LAVORI MANUTENZIONI" = "Direzione Ammninistrativa", 
       #                          "PROGETTI DI RICERCA" = "Direzione Generale", 
       #                          "SERVIZIO ASSICURAZIONE QUALITA'" = "Direzione Generale",
       #                          "SISTEMI INFORMATIVI" = "Direzione Generale",
       #                          "SORVEGLIANZA EPIDEMIOLOGICA EMILIA ROMAGNA" = "Direzione Sanitaria", 
       #                          "SORVEGLIANZA EPIDEMIOLOGICA LOMBARDIA" = "Direzione Sanitaria", 
       #                          "U.O. AFFARI GENERALI E LEGALI" = "Direzione Ammninistrativa", 
       #                          "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Direzione Ammninistrativa", 
       #                          "U.O. GESTIONE SERVIZI CONTABILI" = "Direzione Ammninistrativa", 
       #                          "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE" = "Dipartimento Tutela e  Salute Animale"
       #   ),
       #   CENTRO_DI_COSTO = recode(CENTRO_DI_COSTO, "LABORATORIO DIAGNOSTICA GENERALE, SIEROLOGIA, BIOLOGIA MOLECOLARE E MICROBIOLOGIA" = "SEDE TERRITORIALE DI PIACENZA",
       #                            "LABORATORIO LATTE" = "SEDE TERRITORIALE DI PIACENZA")
       #   ) %>% 
       #   select(Dipartimento, REPARTO, CENTRO_DI_COSTO, Dirigente, Matricola, Nome, Cognome, `Inizio Rapporto`, `Termine Rapporto`) 
       #   #saveRDS(here("programmazione", "data", "processed", "ANAGRAFE2021.rds"))
       
       
       