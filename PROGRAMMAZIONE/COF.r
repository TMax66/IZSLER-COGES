library(readxl)
library(tidyverse)

#bg <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/bg.xlsx")
#anag <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/anagrafe.xlsx")
time <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/presenze.xlsx")

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/IZSLER-COGES/PROGRAMMAZIONE")
bg <- read_excel("bg.xlsx")
anag <- read_excel("anagrafe.xlsx")
time <- read_excel("presenze.xlsx")

#FULL TIME EQUIVALENT####
View(time)

time$hr <- time$Minuti/60

time %>% 
  group_by(Mese, Matricola) %>% 
  summarise(hrm=sum(hr)) %>% 
  left_join(anag, by="Matricola") %>% 
  drop_na() %>% 
  group_by(Categoria,Matricola) %>% 
  summarise(hrm = sum(hrm))


  
#Attività####
bg %>% 
  group_by(settore) %>% 
  summarise(esami=sum(esami, na.rm = T)) %>% 
  arrange(desc(esami)) %>% 
  janitor::adorn_totals(where = "row")

  



attività <- unique(factor(bg$FinalitàConf))

