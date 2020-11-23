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
                              "LABORATORIO LATTE" = "SEDE TERRITORIALE DI PIACENZA", 
                              "LABORATORIO DIAGNOSTICA GENERALE, SIEROLOGIA, BIOLOGIA MOLECOLARE E MICROBIOLOGIA" = "SEDE TERRITORIALE DI PIACENZA")) %>% 
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

# anag19 %>% 
#   select("matricola" = CDMATR, 
#          "sesso" = SESSO, 
#          "dtnasc" = DTNASC, 
#          "categoria" = DEMANSP3, 
#          "hperc" = PCGIUR, 
#          "contratto" = DECOMP) %>% 
#   mutate(hcontr = ifelse( contratto == "COMPARTO SSN", (36*hperc)/100, (38*hperc)/100)) %>% 
#   # filter(contratto == "COMPARTO SSN") %>%
#   left_join(grusigma, by = "matricola") %>% 
#   right_join(hwd, by = "Matricola" ) %>% 
#   group_by(Dipartimento, Reparto, Laboratorio) %>% 
#   summarise(hworked= sum(hworked), 
#             hprev = sum(hcontr*45.6)) %>% 
#   saveRDS(., file = here("programmazione", "data", "processed", "hwd19.rds"))




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
   mutate("FTE-previsto" = ifelse(contratto == "DIRIGENZA", hprev/(38*45.6), hprev/(36*45.6)),
         "FTE-reale" = ifelse(contratto == "DIRIGENZA", hworked/(38*45.6), hworked/(36*45.6))) %>% 
         # "%tempo-utilizzato" = 100*(hworked/hprev),
         # "tempo-medio esame" = hworked/esami,
         # "RxFTEr" = ricavi/`FTE-reale`) %>%
  saveRDS(., file = here("programmazione", "data", "processed", "dati.rds"))









####carico i dati x collegare i ricavi da vp e ai al dataset  complessivo####

vp <- readRDS( here("programmazione", "data", "processed", "vprodotti.rds"))
ai <- readRDS( here("programmazione", "data", "processed", "ainterna19.rds"))
dati <- readRDS( here("programmazione", "data", "processed", "dati.rds"))


vp %>% 
  filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
  mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA",
                          "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
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
  group_by(Dipartimento) %>% 
  summarise(VP = round(sum(`Vendita Prodotti`), 0)) %>% 
  saveRDS(., file = here("programmazione", "shinyapp", "vp.rds"))


 ai %>% 
  filter(Reparto != "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA") %>% 
  mutate(Reparto = recode(Reparto, "VIROLOGIA" = "REPARTO VIROLOGIA",
                          "VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
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
  group_by(Dipartimento) %>% 
  summarise(AI = round(sum(`Attività Interna`),0)) %>% 
   saveRDS(., file = here("programmazione", "shinyapp", "ai.rds"))




dir <- dati %>% 
  filter(contratto == "DIRIGENZA") %>% 
  group_by(Dipartimento) %>% 
  summarise(esami = sum(esami), 
            ricavi = sum(ricavi),
            FTE_d = round(sum(`FTE-reale`),1))
  

comp <- dati %>% 
  filter(contratto == "COMPARTO") %>% 
  group_by(Dipartimento) %>% 
  summarise(esami = sum(esami), 
            ricavi = sum(ricavi),
            FTE_c = round(sum(`FTE-reale`),1))

tabella <- dir %>% 
  bind_cols((comp %>% 
              select(4)), (vp %>% 
                             select(2)), (ai %>% 
                                            select(2))) %>% 
  
  mutate(RT = (ricavi+VP+AI), 
         FTE_t = round((FTE_d+FTE_c),1)) %>% 
  arrange(desc(esami)) %>% 
  adorn_totals(where = "row") %>% 
  mutate( "R-FTE" = round(RT/FTE_t,0) ) %>% 
  select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP, 
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")  
 

ft <- flextable(tabella)
ft <- autofit(ft)
print(ft, preview = "docx")


######################################################################
####grafici#######################################

tabella <- tabella[-5,]
tabella$size<-tabella$N.esami/1000

# tabella$size <-  cut(tabella$size, quantile(tabella$size), include.lowest = F)
# 
#  
# dt<-dt %>% 
#   mutate(size=recode(size, 
#                      `(5,100]`="<100",
#                      `(100,400]`= "100-400",
#                      `(400,800]`="400-800",
#                      `(800,1.2e+03]`="800-1200"))
tabella %>% 
  ggplot(aes(x=RT,y=`R/FTET`,label=Dipartimento, color=size))+
  geom_point(alpha=0.9)+
  # scale_size_discrete("N.esamix1000",range=c(5, 12))+
  # scale_color_manual("N.esamix1000",
  #                    values= c("grey20","steelblue","salmon2","brown1"))+
  geom_hline(yintercept= median(tabella$`R/FTET`), col="red")+
  geom_vline(xintercept= median(tabella$RT), col="blue")+
  labs(x="Ricavi Totali € ", y="Ricavo per FTE €")+
  geom_text_repel(aes(label = Dipartimento))
  #                 size = 2.8, colour="black")+
  # annotate(geom="text",label="Mediana RPA = 40671 €",
  #          x=500000,y=42000, size = 4, colour = "red")+
  # annotate(geom="text",label="Mediana Ricavi Totali = 1110935 €",
  #          x=1300000,y=100000, size = 4, colour = "blue")


  z<-x %>% 
    group_by(Dipartimento, Reparto, Laboratorio, contratto) %>% 
    summarise(matricole = n(),
              orelavorate = sum(hworked))
  
    
    
  