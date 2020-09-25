library(readxl)
library(tidyverse)
library(lubridate)

datiatt <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/BGSOBI2019.xlsx")
anag <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/HR.xlsx")
time <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/personaleBgSoVa.xlsx")

# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/IZSLER-COGES/PROGRAMMAZIONE")
# bg <- read_excel("bg.xlsx")
# anag <- read_excel("anagrafe.xlsx")
# time <- read_excel("presenze.xlsx")



#Attività####

datiatt$dtreg<-as.Date(datiatt$datareg, format="%Y-%m-%d")

datiatt$anno <- year(datiatt$dtreg)
datiatt$mese <- month(datiatt$dtreg)

datiatt$repanalisi2 <- ifelse(datiatt$repanalisi== "Sede Territoriale di Bergamo", "Sede Territoriale di Bergamo", 
                             ifelse(datiatt$repanalisi== "Sede Territoriale di Binago","Sede Territoriale di Binago",
                                    ifelse(datiatt$repanalisi== "Sede Territoriale di Sondrio","Sede Territoriale di Sondrio","Altri reparti")))

#Numero conferimenti 2019 BG-SO ####

nconf<-datiatt %>% 
  filter(anno==2019) %>% 
  group_by(repacc) %>% 
  summarise(totconf = sum(conf, na.rm = T)) %>% 
  select("reparto" = repacc, totconf)


nesami <- datiatt %>% 
  filter(anno==2019) %>% 
  group_by(repanalisi2) %>% 
  summarise(totesami = sum(esami, na.rm = T)) %>% 
  arrange(desc(totesami)) %>% 
  select("reparto" = repanalisi2, totesami)

  
att <-  nconf %>% 
  right_join(nesami, by = "reparto") %>% 
  arrange(desc(totesami)) %>% 
  janitor::adorn_totals(where = "row")








#FULL TIME EQUIVALENT####
View(time)

time$hr <- time$Minuti/60

time %>% 
  group_by(Matricola) %>% 
  summarise(hrm=sum(hr)) %>% 
  left_join(anag, by="Matricola") %>% 
  drop_na() %>% 
  group_by(Categoria,Matricola, Cognome) %>% 
  summarise(hrm = sum(hrm))


  






attività %>% 
  group_by(anno, repanalisi) %>% 
  summarise(Esami = sum(esami = sum(esami, na.rm = T)))



attività %>% 
  filter(repanalisi %in% c("Sede Territoriale di Bergamo", "Sede Territoriale di Sondrio")) %>% 
  group_by(anno, repanalisi) %>% 
    summarise(totes = sum(esami, na.rm = T))








tabella <- bg %>% 
  mutate(mese = month(datareg)) %>% 
  group_by(settore, prova, tecnica, labs) %>% 
  summarise(Esami = sum(esami, na.rm = T)) %>% 
  #select(settore, prova, esami) %>% 
  unique() 

write.csv(tabella, file = "tabella.csv")



bg %>% 
  mutate(mese = month(datareg)) %>% 
  group_by(labs) %>% 
  summarise(Esami = sum(esami, na.rm = T)) %>% 
  janitor::adorn_totals(where = "row") %>% 
  #select(settore, prova, esami) %>% 
  unique() 


 
  


attività <- unique(factor(bg$'Finalità del conferimento (SM)'))
names(bg)
