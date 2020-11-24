library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")

######DATI PER TEMPI ANALSI ######

##tempi esame
tempi <- read_excel(here("programmazione", "data", "raw", "newtempianalisi.xlsx"))
tempi$mp <- substr(tempi$VALORI_MP_REV, start=1, stop = 9)
tempi$VNMP <- paste(tempi$VALORI_VN,tempi$mp, tempi$VALORI_REVISIONE) # <- creo una chiave univoca in tempi tempi$VNMPdup <- duplicated(tempi$VNMP)

## esami singoli
esami <- read_excel(here("programmazione", "data", "raw", "dati2019.xlsx"))
esami <- esami %>% 
  filter(is.na(provagruppi))
esami$MMPP <- substr(esami$mp, start=1, stop = 9)
esami$VNMP <- paste(esami$vn, esami$MMPP, esami$revmp) # <- creo chiave simile a tempi$VNMP per fare collegamento tra esami e tempi
esami$REPARTO <- tolower(esami$Reparto)

esami <- esami %>% 
  mutate(REPARTO = recode(REPARTO, "sede territoriale di milano (is)" = "sede territoriale di milano", 
                          "reparto tecnologie biologiche applicate - batteriologia specializzata" = "reparto tecnologie biologiche applicate", 
                          "reparto tecnologie biologiche applicate - colture cellulari" = "reparto tecnologie biologiche applicate", 
                          "reparto virologia - laboratorio proteomica" = "reparto virologia", 
                          )) %>% 
  filter(!REPARTO %in% c("analisi del rischio ed epidemiologia genomica",
                         "reparto produzione e controllo materiale biologico"))

## esami gruppi####

gruppi <- read_excel(here("programmazione", "data", "raw", "dati2019Gruppi.xlsx"))
gruppi$MMPP <- substr(gruppi$`Descrizione del MP`, start=1, stop = 9)
gruppi$VNMP <- paste(gruppi$`Chiave VN Gruppo`, gruppi$MMPP, gruppi$`Revisione del MP`)
gruppi$REPARTO <- tolower(gruppi$`Reparto che esegue le analisi`)

gruppi <- gruppi %>% 
  mutate(REPARTO = recode(REPARTO,  
                          "reparto tecnologie biologiche applicate - batteriologia specializzata" = "reparto tecnologie biologiche applicate")) %>% 
  filter(!REPARTO %in% c("reparto produzione e controllo materiale biologico"))


gruppi <-  gruppi %>% 
  group_by(REPARTO, VNMP,
          dtreg = `Data di registrazione`) %>% 
  summarise(esami = n()) 


###gruppi+singoli

Esami <- esami %>% 
  select(REPARTO, VNMP, dtreg, esami) %>% 
  union ( (gruppi %>% 
             select(REPARTO,VNMP, dtreg, esami )) )
  

###ore lavorate

hwd19 <- readRDS( here("programmazione", "data", "processed", "hwd19.rds")) # <- dati ore contratto e ore erogate per dip/rep/lab

hwd19$REPARTO <- tolower(hwd19$Laboratorio)

hwd19 <- hwd19 %>% 
  mutate(REPARTO = recode(REPARTO, "sede territoriale di piacenza - parma" = "sede territoriale di piacenza" , 
                          "laboratorio chimica applicata alle tecnologie alimentari" = "reparto chimica degli alimenti e mangimi",
                          "laboratorio contaminanti ambientali" = "reparto chimica degli alimenti e mangimi", 
                          "laboratorio mangimi e tossicologia" = "reparto chimica degli alimenti e mangimi", 
                          "laboratorio residui" = "reparto chimica degli alimenti e mangimi", 
                          "reparto chimico degli alimenti (bologna)" = "bologna (reparto chimico degli alimenti)", 
                          "laboratorio analisi genomiche, laboratorio diagnostica molecolare, ogm" = "reparto tecnologie biologiche applicate",
                          "laboratorio batteriologia specializzata" = "reparto tecnologie biologiche applicate", 
                          "laboratorio colture cellulari, biobanca" = "reparto tecnologie biologiche applicate", 
                          "laboratorio di proteomica e diagnostica tse" = "reparto virologia", 
                          "laboratorio di virologia e sierologia specializzata, microscopia elettronica" = "reparto virologia")) %>% 
  filter(!REPARTO %in% c("laboratorio benessere animale, biochimica clinica, immunologia veterinaria e stabulari",
                         "laboratorio di controllo di prodotti biologici, farmaceutici e convalida di processi produttivi", 
                         "laboratorio produzione terreni", 
                         "laboratorio produzione vaccini e reagenti"))




#####database######
t <- tempi %>% 
  select(VNMP, MinutiComparto, MinutiDirigente) %>% 
  group_by(VNMP) %>% 
  summarise(mincomp = mean(MinutiComparto, na.rm = TRUE), 
            mindir = mean(MinutiDirigente, na.rm = TRUE)) 


Esami %>% 
  left_join(t, by = "VNMP") %>% 
  mutate(tesami = esami*mincomp) %>% 
  group_by(REPARTO) %>% 
  summarise(nesami = sum(esami, na.rm = T),
            tesami = sum(tesami, na.rm = T)/60) %>% 
  left_join(
    (hwd19 %>% 
      group_by(Dipartimento, REPARTO) %>% 
      summarise(hworked = sum(hworked), 
                hprev = sum(hprev))),
    by = "REPARTO") %>% 
  mutate(FTE_previsto = hprev/1641.6, 
         FTE_reale = hworked/1641.6, 
         FTE_necessari = tesami/1641.6) %>% 
  select(Dipartimento, Reparto = REPARTO, "N. analisi" = nesami, "tempo analisi (min)" = tesami, "ore lavorate"= hworked, "ore disponibili" = hprev,
         FTE_previsto, FTE_reale) 
  

##############################################################################
#############################################################################

##### DATI PER TEMPI-ESAMI######

##tempi esame
tempi <- read_excel(here("programmazione", "data", "raw", "newtempianalisi.xlsx"))
tempi$mp <- substr(tempi$VALORI_MP_REV, start=1, stop = 9)
tempi$VNMP <- paste(tempi$VALORI_VN,tempi$mp, tempi$VALORI_REVISIONE) # <- creo una chiave univoca in tempi tempi$VNMPdup <- duplicated(tempi$VNMP)
esami <- read_excel(here("programmazione", "data", "raw",  "dati2019.xlsx"))
esami$MMPP <- substr(esami$mp, start=1, stop = 9)
esami$VNMP <- ifelse(!is.na(esami$vngruppo), paste(esami$vngruppo, esami$MMPP, esami$revmp), paste(esami$vn, esami$MMPP, esami$revmp)) # <- creo chiave simile a tempi$VNMP per fare collegamento tra esami e tempi
esami$REPARTO <- tolower(esami$Reparto)

esami <- esami %>% 
  mutate(REPARTO = recode(REPARTO, "sede territoriale di milano (is)" = "sede territoriale di milano", 
                          "reparto tecnologie biologiche applicate - batteriologia specializzata" = "reparto tecnologie biologiche applicate", 
                          "reparto tecnologie biologiche applicate - colture cellulari" = "reparto tecnologie biologiche applicate", 
                          "reparto virologia - laboratorio proteomica" = "reparto virologia", 
  )) %>% 
  filter(!REPARTO %in% c("analisi del rischio ed epidemiologia genomica",
                         "reparto produzione e controllo materiale biologico"))


hwd19 <- readRDS( here("programmazione", "data", "processed", "hwd19.rds")) # <- dati ore contratto e ore erogate per dip/rep/lab

hwd19$REPARTO <- tolower(hwd19$Laboratorio)

hwd19 <- hwd19 %>% 
  mutate(REPARTO = recode(REPARTO, "sede territoriale di piacenza - parma" = "sede territoriale di piacenza" , 
                          "laboratorio chimica applicata alle tecnologie alimentari" = "reparto chimica degli alimenti e mangimi",
                          "laboratorio contaminanti ambientali" = "reparto chimica degli alimenti e mangimi", 
                          "laboratorio mangimi e tossicologia" = "reparto chimica degli alimenti e mangimi", 
                          "laboratorio residui" = "reparto chimica degli alimenti e mangimi", 
                          "reparto chimico degli alimenti (bologna)" = "bologna (reparto chimico degli alimenti)", 
                          "laboratorio analisi genomiche, laboratorio diagnostica molecolare, ogm" = "reparto tecnologie biologiche applicate",
                          "laboratorio batteriologia specializzata" = "reparto tecnologie biologiche applicate", 
                          "laboratorio colture cellulari, biobanca" = "reparto tecnologie biologiche applicate", 
                          "laboratorio di proteomica e diagnostica tse" = "reparto virologia", 
                          "laboratorio di virologia e sierologia specializzata, microscopia elettronica" = "reparto virologia")) %>% 
  filter(!REPARTO %in% c("laboratorio benessere animale, biochimica clinica, immunologia veterinaria e stabulari",
                         "laboratorio di controllo di prodotti biologici, farmaceutici e convalida di processi produttivi", 
                         "laboratorio produzione terreni", 
                         "laboratorio produzione vaccini e reagenti"))


#####database######
t <- tempi %>%
  select(VNMP, MinutiComparto, MinutiDirigente) %>%
  group_by(VNMP) %>%
  summarise(mincomp = mean(MinutiComparto, na.rm = TRUE),
            mindir = mean(MinutiDirigente, na.rm = TRUE))
 
 

esami %>% 
  left_join(t, by = "VNMP") %>% 
  mutate(tesami = esami*mincomp) %>% 
  group_by(REPARTO) %>% 
  summarise(nesami = sum(esami, na.rm = T),
            tesami = sum(tesami, na.rm = T)/60) %>% 
  left_join(
    (hwd19 %>% 
       group_by(Dipartimento, REPARTO) %>% 
       summarise(hworked = sum(hworked), 
                 hprev = sum(hprev))),
    by = "REPARTO") %>% 
  mutate(FTE_previsto = hprev/1641.6, 
         FTE_reale = hworked/1641.6, 
         FTE_necessari = tesami/1641.6,
         perctana= 100*(tesami/hworked), 
         "%hworked" = 100*(hworked/hprev)) %>% 
  select(Dipartimento, Reparto = REPARTO, "N. analisi" = nesami, "tempo analisi (h)" = tesami, "ore lavorate"= hworked, "ore disponibili" = hprev, "% tempi analisi su h lavorate" = perctana, "% h worked" = "%hworked",
         FTE_previsto, FTE_reale, FTE_necessari) %>% 
  View()





esami %>% 
  semi_join(t, by = "VNMP2") %>% 
  mutate(tesami = esami*mincomp) %>% 
  group_by(REPARTO) %>% 
  summarise(nesami = sum(esami, na.rm = T),
            tesami = sum(tesami, na.rm = T)/60) %>% 
  left_join(
    (hwd19 %>% 
       group_by(Dipartimento, REPARTO) %>% 
       summarise(hworked = sum(hworked), 
                 hprev = sum(hprev))),
    by = "REPARTO") %>% 
  mutate(FTE_previsto = hprev/1641.6, 
         FTE_reale = hworked/1641.6, 
         FTE_necessari = tesami/1641.6) %>% 
  select(Dipartimento, Reparto = REPARTO, "N. analisi" = nesami, "tempo analisi (min)" = tesami, "ore lavorate"= hworked, "ore disponibili" = hprev,
         FTE_previsto, FTE_reale, FTE_necessari) %>% 
  View()


esami %>% 
  group_by(REPARTO, VNMP2) %>% 
summarise(nesami = sum(esami, na.rm = T))

 