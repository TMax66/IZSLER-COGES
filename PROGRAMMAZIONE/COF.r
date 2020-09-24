library(readxl)
library(tidyverse)
library(lubridate)

bg <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/BGSOBI2019.xlsx")
anag <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/anagrafe.xlsx")
time <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/presenze.xlsx")

# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/IZSLER-COGES/PROGRAMMAZIONE")
# bg <- read_excel("bg.xlsx")
# anag <- read_excel("anagrafe.xlsx")
# time <- read_excel("presenze.xlsx")

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


  
#Attività####

bg %>% 
  group_by(repacc) %>% 
  summarise(conf = sum(Conf = sum(conf, na.rm = T))) %>% 
  arrange(desc(conf)) %>% 
  janitor::adorn_totals(where = "row")











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
