library(tidyverse)
library(readxl)
repmat <- read_excel("PROGRAMMAZIONE/APP/repsim.xlsx")
replab <- read_excel("PROGRAMMAZIONE/APP/repsim.xlsx", 
                     sheet = "replab")


df <-repmat %>% 
  left_join(replab, by = "Reparto") %>% 
  mutate(hperc = rep(0, dim(.)[1])) %>% 
  filter(Reparto == "SO") %>% 
  pivot_wider( names_from = "Laboratorio", values_from = hperc, values_fill = 0) %>% 
  mutate(tot = rep(0, dim(.)[1])) %>% 
  rhandsontable::rhandsontable(.)

