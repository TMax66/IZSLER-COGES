library("tidyverse")
library("readxl")
#library("RColorBrewer")
# library("shiny")
# library("shinydashboard")
library("here")
library("janitor")
library("flextable")
library("ztable")
# library("shinyBS")
# library("officer")
# library("fmsb")
library("knitr")

dt <- readRDS( here("programmazione", "shinyapp-in-produzione", "datiSB.rds"))
  
dt %>% 
  group_by( Dipartimento) %>% 
  mutate(FTEDp = 100* prop.table(FTED), 
         FTECp = 100* prop.table(FTEC) ) %>%  
  #group_by(Dipartimento, Valorizzazione) %>% 
  #summarise(FTEDp = sum(FTEDp)) %>% 
   pivot_wider(id_cols = 1:4, 
               names_from = "Dipartimento", values_from = "FTEDp") %>% 
    mutate(total = rowSums(across(where(is.numeric))))%>% 
    filter(total > 0.00000000) %>% 
    arrange(desc(Valorizzazione)) %>% 
    select(-total, -Valorizzazione, -Obiettivo) %>% 
    column_to_rownames(var = "obcod") %>% 
    ztable() %>% 
    makeHeatmap(palette="Blues") %>% print(caption="Table 4. Heatmap Table")
  
