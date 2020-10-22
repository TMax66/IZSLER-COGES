library("tidyverse")
library("networkD3")
library("hrbrthemes")
library("readxl")
library("ggrepel")
library("RColorBrewer")
library("wesanderson")
library("DT")
library("shiny")
library("here")
library("knitr")
library("kableExtra")
library("formattable")
library("shinythemes")
library("rpivotTable")
library(here)
options(scipen = .999)

#dati <- readRDS( here("programmazione", "data", "processed", "dati.rds"))
dati <- readRDS("dati.rds") 
  
dati <- dati %>% 
  filter(!Laboratorio %in% c("LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI",
                             "LABORATORIO PRODUZIONE TERRENI",
                             "LABORATORIO PRODUZIONE VACCINI E REAGENTI",
                             "LABORATORIO COLTURE CELLULARI, BIOBANCA") ) %>%  
  
  mutate(across(where(is.numeric), function(x) round(x, 2)))



