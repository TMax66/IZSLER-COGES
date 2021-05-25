library(shinydashboard)
library(shiny)
library(tidyverse)
library(readxl)
library(ggrepel)
library(hrbrthemes)
options(scipen = .999)
riepilogo <- read_excel("dati.xls", sheet = "riepilogo")
personale<-read_excel("dati3.xls", sheet = "reparti")
personale$comparto<-rowSums(personale[,2:8], na.rm = T)
personale$dirigenza<-rowSums(personale[,9:11], na.rm = T)
personale$personale<-rowSums(personale[,12:13], na.rm = T)

r<-riepilogo %>% 
  group_by(Reparto) %>% 
  summarise(esami=round(sum(N.esami),0), ricavi=round(sum(Valore),0)) 

ai<-riepilogo %>% 
  select(Reparto,`Attività Interna`) %>% 
  drop_na(`Attività Interna`)

vp<-riepilogo %>% 
  select(Reparto,`Vendita Prodotti`) %>% 
  drop_na(`Vendita Prodotti`) 

p<-personale %>% 
  select(1,12:14)


dt<-left_join(r,vp)
dt<-left_join(dt,ai)
dt<-left_join(dt,p)

dt$Rtot<-rowSums(dt[3:5])
dt<-dt %>% 
  mutate(RPA=Rtot/personale)
