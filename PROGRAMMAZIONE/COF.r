library(readxl)
library(tidyverse)
bg <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/bg.xlsx")
anag <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/anagrafe.xlsx")
time <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/presenze.xlsx")



#FULL TIME EQUIVALENT####
View(time)


 
#Attività####
bg <- bg %>% 
  filter(settore !="Controlli Interni Sistema Qualità")

attività <- unique(factor(bg$FinalitàConf))

filtro <- c("Progetto: PRC2018005", "Circuiti interni di laboratorio", "Qualifica/mantenimento qualifica personale")