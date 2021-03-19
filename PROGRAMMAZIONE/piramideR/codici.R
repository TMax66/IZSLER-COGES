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

dati19 <- read_excel(here("programmazione", "piramideR", "pers19.xlsx"), sheet = "ELENCO")


presenze21 <- dati21 %>% 
  mutate(REPARTO = recode(Dislocazione, 
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


# anag19 <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))## anagrafica del 2019 (da qui prendo le matricole)
# grusigma <- read_excel(here("programmazione", "data", "raw", "MatricoleSigmaGRU.xlsx"))
# grusigma <- grusigma %>% 
#   select("matricola" = Matricola, "Matricola" = MatricolaSigma)
# 
# 
# 
# anag19 %>%
#   select("matricola" = CDMATR,
#          "sesso" = SESSO,
#          "dtnasc" = DTNASC,
#          "categoria" = DEMANSP3,
#          "hperc" = PCGIUR,
#          "contratto" = DECOMP) %>%
#   left_join(grusigma, by = "matricola") %>%
#   mutate(matunique = !duplicated(matricola)) %>%
#   filter(matunique == "TRUE") %>%
#   right_join(hwd, by = "Matricola" ) %>%
#   filter(contratto == "DIRIGENZA") %>%
#   select(matricola,Dipartimento, Reparto, Laboratorio, hcontr, hworked ) %>%
#   saveRDS(., file = here("programmazione", "data", "processed", "matrperpubb.rds"))
# 






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
            N.pub = n()) %>% View()
 
  #arrange(desc(MIF)) %>% 
  ggplot(aes(x=Reparto, y=MIF))+
  geom_bar(stat = "identity")+
  coord_flip()


x = fct_infreq(Position)
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
       