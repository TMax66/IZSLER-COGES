library("tidyverse")
library("here")
library("readxl")

#### DATI DA CONTROLLO DI GESTIONE####
attività2019 <- read_excel(sheet = "reparti", here("programmazione", "NUOVA VERSIONE", "data", "raw", "attivita2019.xlsx"))
attività2020 <- read_excel(sheet = "Foglio1", here("programmazione", "NUOVA VERSIONE", "data", "raw", "attivita2020.xlsx"))



#####Dati attività interna e vendita prodotti by dip/rep####
attività2020 %>% 
  group_by(Reparto) %>% 
  summarise(
            VP = sum(`vendita prodotti`, na.rm = TRUE),
            AI = sum(`attività interna`, na.rm = TRUE)) %>% 
  mutate(Anno = rep(2020, nrow(.))) %>% 
  
  bind_rows( 

attività2019 %>% 
  group_by(Reparto) %>% 
  summarise(  
            VP = sum(`vendita prodotti`, na.rm = TRUE),
            AI = sum(`attività interna`, na.rm = TRUE)) %>% 
  mutate(Anno = rep(2019, nrow(.)))

  ) %>% 
  #filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
  mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA",
                          "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "CONTROLLO ALIMENTI" = "REPARTO CONTROLLO ALIMENTI",
                          "PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
                          "CHIMICO DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
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
                                "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
                                "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "Direzione Sanitaria")) %>% 
  saveRDS(., file = here("programmazione", "NUOVA VERSIONE",  "shinyapp", "vpai.rds"))


#####Dati esami e ricavi by dip/rep/lab####
attività2020 %>% 
  group_by(Reparto, Laboratorio) %>% 
  summarise(
    N.esami = sum(n.esami, na.rm = TRUE),
    Ricavi = sum(valore, na.rm = TRUE)) %>% 
  ungroup() %>%  
  mutate(Anno = rep(2020,nrow(.) )) %>% 
  
  bind_rows( 
    
    attività2019 %>% 
      group_by(Reparto, Laboratorio) %>% 
      summarise(  
        N.esami = sum(n.esami, na.rm = TRUE),
        Ricavi = sum(valore, na.rm = TRUE))  %>% 
      ungroup() %>% 
      mutate(Anno = rep(2019, nrow(.)))  
  ) %>% 
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
  #filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
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
  
  saveRDS(., file = here("programmazione", "NUOVA VERSIONE",  "shinyapp", "esamiricavi.rds"))


#####Dati costi by dip/rep####

####DATI ORE LAVORATE MATRICOLE BY DIP/REP####

####DATI PROGETTI DI RICERCA BY DIP/REP####

####DATI PUBBLICAZIONI BY DIP/REP####

####DATI DA PERFORMANCES DA SCHEDA BUDGET####
  