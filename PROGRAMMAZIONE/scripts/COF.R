library("tidyverse")
library("readxl")
library("RColorBrewer")
library("shiny")
library("shinydashboard")
library("here")
library("janitor")
library("here")
library("flextable")
library("shinyBS")
library("officer")

ricerca <- readRDS(here("programmazione", "shinyapp", "ricerca.rds"))


ricerca <-ricerca %>% 
mutate(IF = ifelse(tipologia == "IF ; Int" | tipologia == "IF",  "IF", NA), 
       INT = ifelse(tipologia == "IF ; Int" | tipologia == "Int",  "Int", NA ), 
       NAZ = ifelse(tipologia == "Naz", "Naz", NA), 
       Oth = ifelse(tipologia == "Others" , "Others", NA)) %>% 
  filter(IF == IF) %>% View()
  unique(factor("nr")) %>% 
  count("nr")
 # group_by(Dipartimento) %>% 
  summarise(sIF = n() )
  

 

 x %>% 
   group_by(nr) %>% 
   count(nr) %>% 
   select(nr) %>% 
   nrow()
   
 ricerca %>% 
   group_by(nr) %>% 
   count(nr) %>% 
   select(nr) %>% 
   nrow()
 