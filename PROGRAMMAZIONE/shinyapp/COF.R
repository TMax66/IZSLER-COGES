library("tidyverse")
library("networkD3")
library("hrbrthemes")
library("readxl")
library("ggrepel")
library("RColorBrewer")
library("wesanderson")
library("DT")
library("shiny")
library("shinydashboard")
library("here")
library("knitr")
library("kableExtra")
library("formattable")
library("shinythemes")
library("rpivotTable")
library("janitor")
library("here")
library("flextable")

tizsler %>% 
  filter(Dipartimento != "Total") %>% 
  mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
         "RA" = round(100*(RA/sum(RA)),1), 
         "FTED" = round(100*(FTED/sum(FTED)),1), 
         "FTEC" = round(100*(FTEC/sum(FTEC)),1),
         "RVP" =round(100*(RVP/sum(RVP)),1), 
         "RAI" = round(100*(RAI/sum(RAI)), 1),
         "RT" = round(100*(RT/sum(RT)),1),
         "FTET" = round(100*(FTET/ sum(FTET)), 1),
         "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
  ) %>% 
  select(Dipartimento, Esami, "FTED", "FTEC", "FTET",   "RT", "Ricavo per FTE") %>% 
  pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>% 
  mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT", "Ricavo per FTE"  ))) %>% 
  pivot_wider(names_from = "KPI", values_from = "valore")
