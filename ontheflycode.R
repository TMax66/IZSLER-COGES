library(flexdashboard)
library(tidyverse)
library(networkD3)
library(hrbrthemes)
library(readxl)
library(ggrepel)
library("RColorBrewer")
library(wesanderson)
library(DT)
library(plotly)
library(shiny)
options(scipen = .999)
riepilogo <- read_excel("dati.xls", sheet = "riepilogo")
personale<-read_excel("dati3.xls", sheet = "reparti")
personale$comparto<-rowSums(personale[,2:8], na.rm = T)
personale$dirigenza<-rowSums(personale[,9:11], na.rm = T)
personale$personale<-rowSums(personale[,12:13], na.rm = T)
costi<- read_excel("costi personale.xls")

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

costi<-costi[, c(3,5)]
costi<-unique(costi)
costi<-costi %>% 
  group_by(Reparto) %>% 
  summarise(costi=round(sum(`Costo personale`), 2))

dt<-left_join(r,vp)
dt<-left_join(dt,ai)
dt<-left_join(dt,p)
dt<-left_join(dt, costi)

dt$Rtot<-rowSums(dt[3:5])
dt<-dt %>% 
  mutate(RPA=round(Rtot/personale,0),
         ICP=(round(100*(costi/Rtot),0))
  )





dt[2:12]<- round(apply(dt[,2:12], 2, scale),2)

x<-dt %>% 
  column_to_rownames("Reparto") %>% 
  ztable() %>% 
  makeHeatmap(palette="Blues") %>% print(caption="Table 4. Heatmap Table")
